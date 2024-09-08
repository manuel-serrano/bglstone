public class bague {
    public static int nombre_de_coups = 0;
    public static final int nombre_de_pierres = 28;
    public static final int une_pierre = 1;
    public static final int une_case_vide = 0;
    public static int[] jeu = new int[nombre_de_pierres];

    public static void error(String f, String m, int a) {
	System.out.println(nombre_de_coups + " error " + f + " : " + m + " : " + a);
	System.exit(-1);
    }

    public static void init_jeu() {
	nombre_de_coups = 0;
	for(int i=0; i<nombre_de_pierres; i++) jeu[i] = une_pierre;
    }

    public static void enleve_la_pierre(int n) {
	if(jeu[n-1] != une_pierre)
	    error("bague", "cannot remove a stone from an empty slot", n);
        jeu[n-1] = une_case_vide;
    }

    public static void pose_la_pierre(int n) {
	if(jeu[n-1] !=  une_case_vide)
	    error("bague", "cannot lay a stone on a non empty slot", n);
	jeu[n-1] = une_pierre;
    }

    public static boolean autorise_mouvement(int n) {
	if(n == 1) return(true);
	if(n == 2) return(jeu[0] == une_pierre);
	if(jeu[n - 2] != une_pierre) return(false);
	return(ok(true, 0, n));
    }

    public static boolean ok(boolean b, int i, int n) {
	while(i <= n - 3) {
	    b = b && (jeu[i] == une_case_vide);
	    i = i+1;
	}
	return(b);
    }

    public static void enleve_pierre(int n) {
	nombre_de_coups = nombre_de_coups + 1;
	if(autorise_mouvement(n))
	    enleve_la_pierre(n);
	else
	    error("bague", "forbidden action", n);
    }

    public static void pose_pierre(int n) {
	nombre_de_coups = nombre_de_coups + 1;
	if(autorise_mouvement(n))
	    pose_la_pierre(n);
	else
	    error("bague", "forbidden action", n);
    }

    public static void baguen(int n) {
	if(n == 1) {
	    enleve_pierre(1);
	    return;
	}
	if(n == 2) {
	    enleve_pierre(2);
	    enleve_pierre(1);
	    return;
	}
	baguen(n - 2);
	enleve_pierre(n);
	repose(n - 2);
	baguen(n - 1);
    }

    public static void repose(int n) {
	if(n == 1) {
	    pose_pierre(1);
	    return;
	}
	if(n == 2) {
	    pose_pierre(1);
	    pose_pierre(2);
	    return;
	}
	repose(n - 1);
	baguen(n - 2);
	pose_pierre(n);
	repose(n - 2);
    }
    
    public static void main(String[] argv) {
      init_jeu();
      baguen(nombre_de_pierres);
      System.out.println(nombre_de_coups == 178956970);
    }
}
