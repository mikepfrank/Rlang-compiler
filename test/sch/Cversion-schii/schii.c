/* SCHII: Like SCHI2 except uses only integer multiplication in the main loop.
   SCHI2 kept its state in integers but calculating amounts to add
   using floating-point math. 

   This will be the model for my Pendulum implementation.
   */

#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include <string.h>

/* Physical constants. We'll use MKS (m,kg,s,nt,J,coul...) units. */
#define planck_h (6.626e-34)	/* Planck's constant, in Joule-seconds */
#define light_c (2.998e8)	/* Speed of light in meters/second */
static const double
  hbar = planck_h/(2*M_PI),	/* h/2*pi, also in J-sec */
  elec_m = 9.109e-31,		/* Electron rest mass, in kg */
  elec_q = 1.602e-19,		/* Electron charge (abs.value) in Coulombs */
  coul_const = 8.988e9;		/* 1/4*pi*epsilon_0 (Coulomb's law constant)
				   in nt-m^2/coul^2. */

/* Parameters of simulation.                                       o  */
#define space_width (1e-10)	/* Width of sim space in meters: 1 A. */
#define num_pts (128)		/* Number of discrete space points in sim. */
static const double
  sim_dx = space_width/num_pts, /* delta btw. pts, in meters. */
  sim_dt = 5e-22,		/* Simulated time per step, in secs. */
  init_vel = light_c*0.0,	/* Initial velocity in m/s */
  initial_mu = -space_width/4,	/* initial mean electron pos, rel. to ctr. */
  initial_sigma = space_width/20; /* width of initial hump. */

/* For holding some arrays of size num_pts, in real/imaginary pairs. */
static double
  *energies,			/* Real potential energies at points. */
  *on_real,*on_imag,
  *off_real,*off_imag,	/* On/off diagonal matrix elements, real/imag */
  *Psi_real,*Psi_imag;		/* Real at t, imag at t+1/2. */
  
/* Now, we will use Psi_real and Psi_imag only for translation between
   the internal, integer form, and how it is used externally.  Here is
   the real wave function: */

static int *psiR,*psiI;		/* Real and imaginary integer wave function. */
static double scaleFactor;	/* The value, in the integer range, that a real
				   value of 1.0 translates to. */

static int n_steps = 0;		/* Number of iterations done so far. */
static double total_t = 0;	/* Total simulated time so far. */

static int max_steps = 10000;	/* go a million iterations before reversing */
static int direction = 0;	/* forwards */

#define STEPS_PER_SHOT 1

typedef enum energ_funcs {
  neg_gaus=0, pos_gaus, inv_cutoff, parabolic, const_nonzero, const_zero,
  step_barrier
} energ_func_id;

static energ_func_id which_potential = parabolic;

static const char *energ_func_strs[] = {
  "Negative Gaussian potential well",
  "Positive Gaussian potential bump",
  "Inverse distance well with cutoff",
  "Parabolic well for harmonic oscillator",
  "Constant, non-zero energy level",
  "Constant, zero energy",
  "A step barrier to tunnel through"
};

void print_sim_params() {
  printf("\n");
  printf("SCHROEDINGER SIMULATOR PARAMETERS\n");
  printf("---------------------------------\n");
  printf("\n");
  printf("Width of simulated space is %g meters (%g light-seconds).\n",
	 space_width, space_width/light_c);
  printf("Simulating %d discrete points in space.\n", num_pts);
  printf("Distance between points: %g m (%g ls).\n",sim_dx,
	 sim_dx/light_c);
  printf("Time per simulation step: %g secs (light dist: %g m)\n",
	 sim_dt, sim_dt*light_c);
  printf("Initial electron position: mu=%g m, sigma=%g m.\n",
	 initial_mu, initial_sigma);
  printf("Using potential energy function %d: %s.\n", which_potential,
	 energ_func_strs[which_potential]);
  printf("Initial electron velocity = %g m/s (%g c).\n",
	 init_vel, init_vel/light_c);
  printf("Number of steps to go before reversing: %d.\n",max_steps);
  printf("\n");
}

static double *init_real,*init_imag;

void print_stats() {
  /* Calculate and print some stats of the wavefunction. */
  int this = n_steps&1;
  int i;
  double total_p = 0;
  double mom_real = 0, mom_imag = 0,
    potential = 0,
    kin_real = 0, kin_imag = 0,
    energ_real = 0, energ_imag = 0;
  double dPsi2_real, dPsi2_imag;
  double diff=0;

  for(i=0;i<num_pts;i++){
    int next = (i+1)%num_pts,
      prev = (i-1+num_pts)%num_pts;
    double real = Psi_real[i],
      imag = Psi_imag[i];
    double pd = real*real+imag*imag;
    total_p += pd;
    
    dPsi2_real = Psi_real[next] - Psi_real[prev];
    dPsi2_imag = Psi_imag[next] - Psi_imag[prev];
    mom_real += real*dPsi2_imag - imag*dPsi2_real;
    mom_imag -= real*dPsi2_real + imag*dPsi2_imag;

    potential += pd*energies[i];
  }
  mom_real *= hbar/(2*sim_dx);
  mom_imag *= hbar/(2*sim_dx);

  for(i=0;i<num_pts;i++){
    double dr = Psi_real[i] - init_real[i];
    double di = Psi_imag[i] - init_imag[i];
    diff += dr*dr + di*di;
  }
  diff /= num_pts;

  printf("Cycle = %d, t = %g. Total P = %g.\n",n_steps,total_t,total_p);
  printf("   Mean squared diff. from init. state = %g. (RMS = %g)\n",
	 diff, sqrt(diff));
  printf("   Momentum = (%g + i %g) kg m/s.\n",mom_real,mom_imag);
  printf("   Velocity = (%g + i %g) m/s.\n",
	 mom_real/elec_m,mom_imag/elec_m);
  printf("            = (%g + i %g) c.\n",
	 mom_real/(elec_m*light_c),mom_imag/(elec_m*light_c));
  printf("  Potential = %g J (%g eV)\n", potential, potential/1.602e-19);
}

/* Gaussian (normal) distribution, non-normalized. */
double normal(double x,double mu,double sigma)
{
  double d = (x-mu)/sigma;
  return exp(-0.5*d*d);
}

double *malloc_doubles(){
  return (double *)calloc(num_pts,sizeof(double));
}

int *malloc_ints(){
  return (int *)calloc(num_pts,sizeof(int));
}

/* x should now be a real position in space */
double energy(double x){
  switch(which_potential){
  case neg_gaus:
    /* Negative Gaussian potential well. */
    return - 3e-15 * normal(x,0,space_width/10);
  case pos_gaus:
    /* Positive Gaussian potential bump. */
    return 5e-15 * normal(x,0,space_width/10);
  case inv_cutoff:
    /* Well where energy drops with inverse distance from center, down
       to a cutoff threshold. */
    {
      double ax = (x<0?-x:x);
      double p;
      p = - (coul_const*(elec_q*elec_q)/ax);
      if (p > -4e-14) {
	return p;
      } else {
	return -4e-14;
      }
    }
  case parabolic:
    /* Parabolic well for harmonic oscillator. */
    return x*x*1e+6;
  case const_nonzero:
    /* Constant, nonzero energy.  Apparent wave rotation rate differs
       from zero-energy case.*/
    return -24e-15;
  case const_zero:
    /* Constant, zero energy. */
    return 0;
  case step_barrier:
    /* A step barrier through which to tunnel. */
    if (x < space_width * 0.15) {
      return 0;
    } else if (x < space_width * 0.18) {
      return 1e-15;
    } else {
      return 0;
    }
  }
  return 0;
}

static double epsilon;
static double *alphas;
static int *alphasi;
static int epsiloni;

void cache_energies() {
  int i;
  /* epsilon: an angle that roughly indicates how much of a
     point's amplitude gets spread to its neighboring points per time
     step.  A function of sim dt and dx parameters only. */
  epsilon = hbar*sim_dt/(elec_m*sim_dx*sim_dx);
  epsiloni = (int)((unsigned)(0x80000000) * epsilon);
  printf("Sim epsilon angle: %g radians (%g of a circle).\n",
	 epsilon, epsilon/(2*M_PI));
  printf("Integer epsilon: %d\n",epsiloni);
  energies = malloc_doubles();
  on_real = malloc_doubles();
  on_imag = malloc_doubles();
  off_real = malloc_doubles();
  off_imag = malloc_doubles();
  alphas = malloc_doubles();
  alphasi = malloc_ints();
  printf("Integer alphas:\n");
  for(i=0;i<num_pts;i++){
    double x = (i - num_pts/2.0)*sim_dx; /* Position in space. */
    double alpha;
    energies[i] = energy(x);
    /* alpha: at this particular point, what's the absolute phase rotation
       angle for on-diagonal.  Energy makes a contribution. */
    alpha = epsilon + energies[i]*sim_dt/hbar;
    /* A = exp(i*Atheta) */
    on_real[i] = cos(epsilon) * cos(-alpha);
    on_imag[i] = cos(epsilon) * sin(-alpha);
    /* What's the phase rotation for off-diag (neighbors). */
    off_real[i] = sin(epsilon) * cos(M_PI/2 - alpha);
    off_imag[i] = sin(epsilon) * sin(M_PI/2 - alpha);
    
    alphas[i] = alpha;
    alphasi[i] = (int)((unsigned)(0x80000000)*alphas[i]*2);
    printf("%d ",alphasi[i]);
  }
  printf("\n");
}

void init_wave() {
  double *probs = malloc_doubles();
  int i;
  double tprob;
  double lambda;
  Psi_real = malloc_doubles();
  Psi_imag = malloc_doubles();
  psiR = malloc_ints();
  psiI = malloc_ints();
  init_real = malloc_doubles();
  init_imag = malloc_doubles();
  tprob = 0;
  for(i=0;i<num_pts;i++){
    double x = (i - num_pts/2.0)*sim_dx;
    /* Unnormalized initial probability of finding electron here. */
    double p = normal(x,initial_mu,initial_sigma);
    probs[i] = p;
    tprob += p;
  }
  for(i=0;i<num_pts;i++){
    probs[i] /= tprob;
  }
  lambda = planck_h/(elec_m*init_vel);
  printf("Initial de Broglie wavelength is %g m (%g ls).\n",
	 lambda,lambda/light_c);
  for(i=0;i<num_pts;i++){
    double x = (i - num_pts/2.0)*sim_dx;
    Psi_real[i] = sqrt(probs[i]) * cos(x/lambda*2*M_PI);
    Psi_imag[i] = sqrt(probs[i]) * sin(x/lambda*2*M_PI);
  }
  {
    double maxval = 0;
    double absval;
    for(i=0;i<num_pts;i++){
      absval = Psi_real[i];
      absval = (absval<0)?-absval:absval;
      if (absval>maxval) maxval=absval;
      absval = Psi_imag[i];
      absval = (absval<0)?-absval:absval;
      if (absval>maxval) maxval=absval;
    }
    printf("The maximum absolute value initially is: %g.\n",maxval);
    /* We'll scale our integers so that they can hold values up to
       almost twice the largest initial value before they incur
       an overflow. */
    scaleFactor = (1<<30)/maxval;
    printf("Therefore the scale factor will be: %g.\n",scaleFactor);
  }
  /* Convert to integers. */
  printf("Integer psis:\n");
  for(i=0;i<num_pts;i++){
    psiR[i] = Psi_real[i]*scaleFactor;
    psiI[i] = Psi_imag[i]*scaleFactor;
    printf("%d+%di ",psiR[i],psiI[i]);
  }
  printf("\n");
  /* Convert back to doubles for convenience. */
  for(i=0;i<num_pts;i++){
    Psi_real[i] = init_real[i] = psiR[i]/scaleFactor;
    Psi_imag[i] = init_imag[i] = psiI[i]/scaleFactor;
  }
}

void sim_init () {
  print_sim_params();
  cache_energies();
  init_wave();
  print_stats();
}

int signed_mult_frac(int m1,int m2)
{
  int pos,prod=0;
  unsigned int mask = 1<<31;
  int m1p=m1,m2p=m2;
  if (m1<0) m1p = -m1p;
  if (m2<0) m2p = -m2p;
  for(pos=1;pos<32;pos++){
    mask >>= 1;
    if (m1p&mask)
      prod += m2p>>pos;
  }
  if (m1<0) prod = -prod;
  if (m2<0) prod = -prod;
  return prod;
}

double function(int *vec,int i){
  int j,k;
  j = i+1; if (j==-1) j=num_pts-1; else if (j==num_pts) j=0;
  k = i-1; if (k==-1) k=num_pts-1; else if (k==num_pts) k=0;
  return
    signed_mult_frac(alphasi[i],vec[i])
    - signed_mult_frac(epsiloni,vec[j])
    - signed_mult_frac(epsiloni,vec[k]);
}

void check_output() {
  int i;
  char buf[80];
  int bad = 0;
  int align;
  for(i=0;i<num_pts;i++){
    int pend_psiRi;
    gets(buf);			/* Should be 0 */
    align = atoi(buf);
    if (align != 0) {
      printf("Misalignment at step %d, point %d real: %d instead of 0.\n",
	     n_steps,i,align);
    }
    gets(buf);			/* Should be psiR[i] */
    pend_psiRi = atoi(buf);
    if (pend_psiRi != psiR[i]) {
      printf("Difference at step %d, point %d real: %d instead of %d.\n",
	     n_steps,i,pend_psiRi,psiR[i]);
      bad = 1;
    }
  }
  gets(buf);			/* Should be 1 */
  align = atoi(buf);
  if (align != 1) {
    printf("Misalignment after step %d reals: %d instead of 1.\n",
	   n_steps,align);
  }
  for(i=0;i<num_pts;i++){
    int pend_psiIi;
    gets(buf);			/* Should be 0 */
    align = atoi(buf);
    if (align != 0) {
      printf("Misalignment at step %d, point %d imag: %d instead of 0.",
	     n_steps,i,align);
    }
    gets(buf);			/* Should be psiR[i] */
    pend_psiIi = atoi(buf);
    if (pend_psiIi != psiI[i]) {
      printf("Difference at step %d, point %d imag: %d instead of %d.",
	     n_steps,i,pend_psiIi,psiI[i]);
      bad = 1;
    }
  }
  if (bad==0) {
    printf("Step %d completed perfectly by Pendulum.\n",n_steps);
  }
  gets(buf);			/* Should be 1 */
  align = atoi(buf);
  if (align != 1) {
    printf("Misalignment after step %d imags: %d instead of 1.\n",
	   n_steps,align);
  }
}

void step_forwards() {
  int i;
  for(i=0;i<num_pts;i++)
    psiR[i] += (int)(function(psiI,i));
  for(i=0;i<num_pts;i++)
    psiI[i] -= (int)(function(psiR,i));
  for(i=0;i<num_pts;i++){
    Psi_real[i] = psiR[i]/scaleFactor;
    Psi_imag[i] = psiI[i]/scaleFactor;
  }
  n_steps++;
  total_t+=sim_dt;
  check_output();
}

void step_back() {
  int i;
  n_steps--;
  for(i=0;i<num_pts;i++)
    psiI[i] += (int)(function(psiR,i));
  for(i=0;i<num_pts;i++)
    psiR[i] -= (int)(function(psiI,i));
  for(i=0;i<num_pts;i++){
    Psi_real[i] = psiR[i]/scaleFactor;
    Psi_imag[i] = psiI[i]/scaleFactor;
  }
  total_t-=sim_dt;
}

void sim_step() {
  if (direction == 0) {
    step_forwards();
  } else {
    step_back();
  }
  if (direction == 1 && n_steps == 0) {
    printf("\nPresumably back to initial state.\n");
    print_stats();
    printf("Now turning and going forwards.\n");
    direction = 0;
  } else if (direction == 0 && n_steps == max_steps) {
    printf("\nCompleted %d steps.\n", max_steps);
    print_stats();
    direction = 1;
    printf("Now turning around and going backwards.\n");
  }
}

/*----------------------------------------------------------------------*/
/* Graphics. */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include "icon.bitmap"
#define BITMAPDEPTH 1
static Display *display;
static int screen;

static double *prev_real, *prev_imag;

static double maxv;
static double maxp;

void init_graphics() {
  int i;
  int this = n_steps&1;
  prev_real = malloc_doubles();
  prev_imag = malloc_doubles();
  maxv = 0;
  maxp = 0;
  for(i=0;i<num_pts;i++){
    double absv;
    absv = Psi_real[i];
    absv = (absv<0)?-absv:absv;
    if (absv > maxv) maxv = absv;
    absv = Psi_imag[i];
    absv = (absv<0)?-absv:absv;
    if (absv > maxv) maxv = absv;
    absv = Psi_real[i]*Psi_real[i]
      + Psi_imag[i]*Psi_imag[i];
    if (absv > maxp) maxp = absv;
  }
}

void draw_graphics(win,gc,window_width,window_height,gce,gcreal,gcimag)
     Window win;
     GC gc;
     unsigned window_width, window_height;
     GC gce,gcreal,gcimag;
{
  double ght = window_height/2;	/* How much Y space per graph */
  unsigned c1 = ght/2;		/* origin y for top graph */
  unsigned c2 = window_height;	/* origin y for bottom graph */
  int this = n_steps&1;		/* which Psi is current */
  int i;
  
  for(i=0;i<num_pts;i++){
    unsigned x = i*window_width/num_pts;
    double prevReal = prev_real[i],
      prevImag = prev_imag[i],
      thisReal = Psi_real[i],
      thisImag = Psi_imag[i];
    double prevProb = prevReal*prevReal + prevImag*prevImag,
      thisProb = thisReal*thisReal + thisImag*thisImag;
    int prY = (int)((prevReal/maxv)*ght*0.5);
    int piY = (int)((prevImag/maxv)*ght*0.5);
    int ppY = (int)((prevProb/maxp)*ght);
    int trY = (int)((thisReal/maxv)*ght*0.5);
    int tiY = (int)((thisImag/maxv)*ght*0.5);
    int tpY = (int)((thisProb/maxp)*ght);

    /* Similar to above calculation, but for point next to us on the right.*/
    unsigned j = (i+1)%num_pts;
    unsigned xj = (i+1)*window_width/num_pts;
    double RprevReal = prev_real[j],
      RprevImag = prev_imag[j],
      RthisReal = Psi_real[j],
      RthisImag = Psi_imag[j];
    double RprevProb = RprevReal*RprevReal + RprevImag*RprevImag,
      RthisProb = RthisReal*RthisReal + RthisImag*RthisImag;
    int RprY = (int)((RprevReal/maxv)*ght*0.5);
    int RpiY = (int)((RprevImag/maxv)*ght*0.5);
    int RppY = (int)((RprevProb/maxp)*ght);
    int RtrY = (int)((RthisReal/maxv)*ght*0.5);
    int RtiY = (int)((RthisImag/maxv)*ght*0.5);
    int RtpY = (int)((RthisProb/maxp)*ght);

    /* Erase old Psi at this pos. */
    XDrawLine(display,win,gce,x,c1-prY,xj,c1-RprY);
    XDrawLine(display,win,gce,x,c1-piY,xj,c1-RpiY);
    /* Draw new Psi. */
    XDrawLine(display,win,gcreal,x,c1-trY,xj,c1-RtrY);
    XDrawLine(display,win,gcimag,x,c1-tiY,xj,c1-RtiY);
    /* Erase old prob and draw new. */
    XDrawLine(display,win,gce,x,c2-ppY,xj,c2-RppY);
    XDrawLine(display,win,gc,x,c2-tpY,xj,c2-RtpY);
    /* Draw energy function. */
    XDrawPoint(display,win,gc,x,(int)(c2-(ght*0.5)-ght*energies[i]*1e14));
  }
  for(i=0;i<num_pts;i++){
    prev_real[i] = Psi_real[i];
    prev_imag[i] = Psi_imag[i];
  }
  XFlush(display);
}

/*----------------------------------------------------------------------*/
/* Pretty much everything below here is uninteresting X interfacing
   stuff. */

get_GC(Window win, GC *gc, XFontStruct *font_info, int foo) {
  unsigned long valuemask = 0;	/* ignore XGCvalues and use defaults */
  XGCValues values;
  unsigned int line_width = 1;
  int line_style = LineSolid;
  int cap_style = CapButt;
  int join_style = JoinRound;
  int dash_offset = 0;
  static char dash_list[] = {
    12, 24 };
  int list_length = 2;

  /* Create default graphics context */
  *gc = XCreateGC(display,win,valuemask,&values);

  /* specify font */
  XSetFont(display,*gc,font_info->fid);

  {
    XColor sdr,edr;
  if (foo == 1) {
    XSetForeground(display,*gc,WhitePixel(display,screen));
  } else if (foo == 0) {
    XSetForeground(display,*gc,BlackPixel(display,screen));
  } else if (foo == 2) {
    XAllocNamedColor(display,DefaultColormap(display,screen),"cyan",
		&sdr,&edr);
    XSetForeground(display,*gc,edr.pixel);
  } else if (foo == 3) {
    XAllocNamedColor(display,DefaultColormap(display,screen),"yellow",
		&sdr,&edr);
    XSetForeground(display,*gc,edr.pixel);
  }}

  /* set line attributes */
  XSetLineAttributes(display,*gc,line_width,line_style,cap_style,
		     join_style);

  /* set dashes to be line_width in length */
  XSetDashes(display,*gc,dash_offset,dash_list,list_length);
}

load_font(XFontStruct **font_info) {
  char *fontname = "9x15";

  /* Access font */
  if ((*font_info = XLoadQueryFont(display,fontname)) == NULL) {
    fprintf(stderr,"Basic: Cannot open 9x15 font\n");
    exit(-1);
  }
}

int main(argc,argv)
     int argc;
     char **argv;
{
  Window win;
  unsigned width, height;	/* window size */
  int x = 0, y = 0;		/* window position */
  unsigned border_width = 4;	/* border four pixels wide */
  unsigned display_width, display_height;
  char *window_name = "Schroedinger Wave Simulator";
  char *icon_name = "schroed";
  Pixmap icon_pixmap;
  XSizeHints size_hints;
  XEvent report;
  GC gc,gce,gcreal,gcimag;
  XFontStruct *font_info;
  char *display_name = NULL;
  int i;

  /* connect to X server */
  if ( (display=XOpenDisplay(display_name)) == NULL ) {
    fprintf(stderr,
	    "cannot connect to X server %s\n",
	    XDisplayName(display_name));
    exit(-1);
  }

  sim_init();
  init_graphics();

  /* get screen size from display structure macro */
  screen = DefaultScreen(display);

  display_width = DisplayWidth(display,screen);
  display_height = DisplayHeight(display,screen);

  /* size window with enough room for text */
  width = display_width/3, height = display_height/3;

  /* create opaque window */
  win = XCreateSimpleWindow(display,RootWindow(display,screen),
			    x,y,width,height,border_width,
			    WhitePixel(display,screen),
			    BlackPixel(display,screen));

  /* Create pixmap of depth 1 (bitmap) for icon */
  icon_pixmap = XCreateBitmapFromData(display, win, icon_bitmap_bits,
				      icon_bitmap_width,
				      icon_bitmap_height);

  /* initialize size hint property for window manager */
  size_hints.flags = PPosition | PSize | PMinSize;
  size_hints.x = x;
  size_hints.y = y;
  size_hints.width = width;
  size_hints.height = height;
  size_hints.min_width = 175;
  size_hints.min_height = 125;

  /* set properties for window manager (always before mapping) */
  XSetStandardProperties(display,win,window_name,icon_name,
			 icon_pixmap,argv,argc,&size_hints);

  /* Select event types wanted */
  XSelectInput(display,win, ExposureMask | KeyPressMask |
	       ButtonPressMask | StructureNotifyMask);

  load_font(&font_info);

  /* create GC for text and drawing */
  get_GC(win, &gc, font_info, 1);
  get_GC(win, &gce, font_info, 0);
  get_GC(win, &gcreal, font_info, 2);
  get_GC(win, &gcimag, font_info, 3);

  /* Display window */
  XMapWindow(display,win);

  while (1) {			/* Event loop. */
    int i;
    XNextEvent(display,&report);
    switch(report.type) {
    case Expose:
      /* get rid of all other Expose events on the queue */
      while (XCheckTypedEvent(display, Expose, &report));
      draw_graphics(win, gc, width, height, gce, gcreal, gcimag);
      for(i=0;i<STEPS_PER_SHOT;i++)
	sim_step();
      /*print_stats();
	sleep(1);*/
      XClearArea(display,win,0,0,1,1,1);
      break;
    case ConfigureNotify:
      width = report.xconfigure.width;
      height = report.xconfigure.height;
      XClearArea(display,win,0,0,width,height,1);
      break;
    case KeyPress:
      print_stats();
      break;
    case ButtonPress:
      XUnloadFont(display,font_info->fid);
      XFreeGC(display,gc);
      XFreeGC(display,gce);
      XFreeGC(display,gcreal);
      XFreeGC(display,gcimag);
      XCloseDisplay(display);
      exit(1);
    default:
      break;
    }
  }
  return 0;
}

