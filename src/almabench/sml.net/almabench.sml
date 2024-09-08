structure almabench
  : sig val main: string option array option -> unit
    end =
struct
  open Array

  fun print_vector v =
    (print "#(";
     Array.app (fn x => (print( Real.toString x ); print " ")) v;
     print ")")

  fun print_matrix m =
    (print "#(";
     Array.app print_vector m;
     print ")")

  val pic         = 3.14159265358979323846
  val j2000       = 2451545.0
  val jcentury    = 36525.0
  val jmillenia   = 365250.0
  val twopi       = 2.0 * pic
  val a2r         = pic / 648000.0
  val r2h         = 12.0 / pic
  val r2d         = 180.0 / pic
  val gaussk      = 0.01720209895

  (* number of days to include in test *)
  val test_loops  = 4
  val test_length = 36525

  (* sin and cos of j2000 mean obliquity (iau 1976) *)
  val sineps      = 0.3977771559319137
  val coseps      = 0.9174820620691818

  val amas        = fromList [ 6023600.0, 408523.5, 328900.5, 3098710.0, 1047.355, 3498.5, 22869.0, 19314.0 ]

  (*
   * tables giving the mean keplerian elements, limited to t**2 terms:
   *        a       semi-major axis (au)
   *        dlm     mean longitude (degree and arcsecond)
   *        e       eccentricity
   *        pi      longitude of the perihelion (degree and arcsecond)
   *        dinc    inclination (degree and arcsecond)
   *        omega   longitude of the ascending node (degree and arcsecond)
   *)
  val a = fromList [ fromList [  0.3870983098,            0.0,      0.0 ],
                     fromList [  0.7233298200,            0.0,      0.0 ],
                     fromList [  1.0000010178,            0.0,      0.0 ],
                     fromList [  1.5236793419,          3e~10,      0.0 ],
                     fromList [  5.2026032092,      19132e~10,  ~39e~10 ],
                     fromList [  9.5549091915,  ~0.0000213896,  444e~10 ],
                     fromList [ 19.2184460618,      ~3716e~10,  979e~10 ],
                     fromList [ 30.1103868694,     ~16635e~10,  686e~10 ] ]
  val dlm = fromList [ fromList [ 252.25090552,  5381016286.88982,   ~1.92789 ],
                       fromList [ 181.97980085,  2106641364.33548,    0.59381 ],
                       fromList [ 100.46645683,  1295977422.83429,   ~2.04411 ],
                       fromList [ 355.43299958,   689050774.93988,    0.94264 ],
                       fromList [ 34.35151874,   109256603.77991,  ~30.60378 ],
                       fromList [ 50.07744430,    43996098.55732,   75.61614 ],
                       fromList [ 314.05500511,    15424811.93933,   ~1.75083 ],
                       fromList [ 304.34866548,     7865503.20744,    0.21103 ] ]
  val e = fromList [ fromList [ 0.2056317526,   0.0002040653,     ~28349e~10 ],
                     fromList [ 0.0067719164,  ~0.0004776521,      98127e~10 ],
                     fromList [ 0.0167086342,  ~0.0004203654,  ~0.0000126734 ],
                     fromList [ 0.0934006477,   0.0009048438,     ~80641e~10 ],
                     fromList [ 0.0484979255,   0.0016322542,  ~0.0000471366 ],
                     fromList [ 0.0555481426,  ~0.0034664062,  ~0.0000643639 ],
                     fromList [ 0.0463812221,  ~0.0002729293,   0.0000078913 ],
                     fromList [ 0.0094557470,   0.0000603263,            0.0 ] ]
  val pi = fromList [ fromList [   77.45611904,   5719.11590,    ~4.83016 ],
                      fromList [ 131.56370300,    175.48640,  ~498.48184 ],
                      fromList [ 102.93734808,  11612.35290,    53.27577 ],
                      fromList [ 336.06023395,  15980.45908,   ~62.32800 ],
                      fromList [ 14.33120687,   7758.75163,   259.95938 ],
                      fromList [ 93.05723748,  20395.49439,   190.25952 ],
                      fromList [  173.00529106,   3215.56238,   ~34.09288 ],
                      fromList [  48.12027554,   1050.71912,    27.39717 ] ]
  val dinc = fromList [ fromList [  7.00498625,  ~214.25629,    0.28977 ],
                        fromList [   3.39466189,   ~30.84437,  ~11.67836 ],
                        fromList [          0.0,   469.97289,   ~3.35053 ],
                        fromList [   1.84972648,  ~293.31722,   ~8.11830 ],
                        fromList [   1.30326698,   ~71.55890,   11.95297 ],
                        fromList [   2.48887878,    91.85195,  ~17.66225 ],
                        fromList [   0.77319689,   ~60.72723,    1.25759 ],
                        fromList [   1.76995259,     8.12333,    0.08135 ] ]
  val omega = fromList [ fromList [   48.33089304,   ~4515.21727,   ~31.79892 ],
                         fromList [   76.67992019,  ~10008.48154,   ~51.32614 ],
                         fromList [  174.87317577,   ~8679.27034,    15.34191 ],
                         fromList [   49.55809321,  ~10620.90088,  ~230.57416 ],
                         fromList [  100.46440702,    6362.03561,   326.52178 ],
                         fromList [  113.66550252,   ~9240.19942,   ~66.23743 ],
                         fromList [   74.00595701,    2669.15033,   145.93964 ],
                         fromList [  131.78405702,    ~221.94322,    ~0.78728 ] ]

  (* tables for trigonometric terms to be added to the mean elements
     of the semi~major axes. *)
  val kp = fromList [ fromList [ 69613.0,  75645.0, 88306.0, 59899.0, 15746.0, 71087.0, 142173.0,  3086.0,    0.0 ],
                      fromList [ 21863.0,  32794.0, 26934.0, 10931.0, 26250.0, 43725.0,  53867.0, 28939.0,    0.0 ],
                      fromList [ 16002.0,  21863.0, 32004.0, 10931.0, 14529.0, 16368.0,  15318.0, 32794.0,    0.0 ],
                      fromList [  6345.0,   7818.0, 15636.0,  7077.0,  8184.0, 14163.0,   1107.0,  4872.0,    0.0 ],
                      fromList [  1760.0,   1454.0,  1167.0,   880.0,   287.0,  2640.0,     19.0,  2047.0, 1454.0 ],
                      fromList [   574.0,      0.0,   880.0,   287.0,    19.0,  1760.0,   1167.0,   306.0,  574.0 ],
                      fromList [   204.0,      0.0,   177.0,  1265.0,     4.0,   385.0,    200.0,   208.0,  204.0 ],
                      fromList [     0.0,    102.0,   106.0,     4.0,    98.0,  1367.0,    487.0,   204.0,    0.0 ] ]
  val ca = fromList [ fromList [       4.0,    ~13.0,    11.0,    ~9.0,    ~9.0,    ~3.0,    ~1.0,     4.0,    0.0 ],
                      fromList [    ~156.0,     59.0,   ~42.0,     6.0,    19.0,   ~20.0,   ~10.0,   ~12.0,    0.0 ],
                      fromList [      64.0,   ~152.0,    62.0,    ~8.0,    32.0,   ~41.0,    19.0,   ~11.0,    0.0 ],
                      fromList [     124.0,    621.0,  ~145.0,   208.0,    54.0,   ~57.0,    30.0,    15.0,    0.0 ],
                      fromList [  ~23437.0,  ~2634.0,  6601.0,  6259.0, ~1507.0, ~1821.0,  2620.0, ~2115.0,~1489.0 ],
                      fromList [   62911.0,~119919.0, 79336.0, 17814.0,~24241.0, 12068.0,  8306.0, ~4893.0, 8902.0 ],
                      fromList [  389061.0,~262125.0,~44088.0,  8387.0,~22976.0, ~2093.0,  ~615.0, ~9720.0, 6633.0 ],
                      fromList [ ~412235.0,~157046.0,~31430.0, 37817.0, ~9740.0,   ~13.0, ~7449.0,  9644.0,    0.0 ] ]
  val sa = fromList [ fromList [     ~29.0,    ~1.0,     9.0,     6.0,    ~6.0,     5.0,     4.0,     0.0,    0.0 ],
                      fromList [     ~48.0,  ~125.0,   ~26.0,   ~37.0,    18.0,   ~13.0,   ~20.0,    ~2.0,    0.0 ],
                      fromList [    ~150.0,   ~46.0,    68.0,    54.0,    14.0,    24.0,   ~28.0,    22.0,    0.0 ],
                      fromList [    ~621.0,   532.0,  ~694.0,   ~20.0,   192.0,   ~94.0,    71.0,   ~73.0,    0.0 ],
                      fromList [  ~14614.0,~19828.0, ~5869.0,  1881.0, ~4372.0, ~2255.0,   782.0,   930.0,  913.0 ],
                      fromList [  139737.0,     0.0, 24667.0, 51123.0, ~5102.0,  7429.0, ~4095.0, ~1976.0,~9566.0 ],
                      fromList [ ~138081.0,     0.0, 37205.0,~49039.0,~41901.0,~33872.0,~27037.0,~12474.0,18797.0 ],
                      fromList [       0.0, 28492.0,133236.0, 69654.0, 52322.0,~49577.0,~26430.0, ~3593.0,    0.0 ] ]

  (* tables giving the trigonometric terms to be added to the mean elements of
     the mean longitudes . *)
  val kq = fromList [ fromList [  3086.0, 15746.0, 69613.0, 59899.0, 75645.0, 88306.0, 12661.0, 2658.0,  0.0,   0.0 ],
                      fromList [ 21863.0, 32794.0, 10931.0,    73.0,  4387.0, 26934.0,  1473.0, 2157.0,  0.0,   0.0 ],
                      fromList [    10.0, 16002.0, 21863.0, 10931.0,  1473.0, 32004.0,  4387.0,   73.0,  0.0,   0.0 ],
                      fromList [    10.0,  6345.0,  7818.0,  1107.0, 15636.0,  7077.0,  8184.0,  532.0, 10.0,   0.0 ],
                      fromList [    19.0,  1760.0,  1454.0,   287.0,  1167.0,   880.0,   574.0, 2640.0, 19.0,1454.0 ],
                      fromList [    19.0,   574.0,   287.0,   306.0,  1760.0,    12.0,    31.0,   38.0, 19.0, 574.0 ],
                      fromList [     4.0,   204.0,   177.0,     8.0,    31.0,   200.0,  1265.0,  102.0,  4.0, 204.0 ],
                      fromList [     4.0,   102.0,   106.0,     8.0,    98.0,  1367.0,   487.0,  204.0,  4.0, 102.0 ] ]
  val cl = fromList [ fromList [      21.0,   ~95.0, ~157.0,   41.0,   ~5.0,   42.0,   23.0,   30.0,     0.0,    0.0 ],
                      fromList [    ~160.0,  ~313.0, ~235.0,   60.0,  ~74.0,  ~76.0,  ~27.0,   34.0,     0.0,    0.0 ],
                      fromList [    ~325.0,  ~322.0,  ~79.0,  232.0,  ~52.0,   97.0,   55.0,  ~41.0,     0.0,    0.0 ],
                      fromList [    2268.0,  ~979.0,  802.0,  602.0, ~668.0,  ~33.0,  345.0,  201.0,   ~55.0,    0.0 ],
                      fromList [    7610.0, ~4997.0,~7689.0,~5841.0,~2617.0, 1115.0, ~748.0, ~607.0,  6074.0,  354.0 ],
                      fromList [  ~18549.0, 30125.0,20012.0, ~730.0,  824.0,   23.0, 1289.0, ~352.0,~14767.0,~2062.0 ],
                      fromList [ ~135245.0,~14594.0, 4197.0,~4030.0,~5630.0,~2898.0, 2540.0, ~306.0,  2939.0, 1986.0 ],
                      fromList [   89948.0,  2103.0, 8963.0, 2695.0, 3682.0, 1648.0,  866.0, ~154.0, ~1963.0, ~283.0 ] ]
  val sl = fromList [ fromList [   ~342.0,   136.0,  ~23.0,   62.0,   66.0,  ~52.0,  ~33.0,   17.0,     0.0,    0.0 ],
                      fromList [    524.0,  ~149.0,  ~35.0,  117.0,  151.0,  122.0,  ~71.0,  ~62.0,     0.0,    0.0 ],
                      fromList [   ~105.0,  ~137.0,  258.0,   35.0, ~116.0,  ~88.0, ~112.0,  ~80.0,     0.0,    0.0 ],
                      fromList [    854.0,  ~205.0, ~936.0, ~240.0,  140.0, ~341.0,  ~97.0, ~232.0,   536.0,    0.0 ],
                      fromList [ ~56980.0,  8016.0, 1012.0, 1448.0,~3024.0,~3710.0,  318.0,  503.0,  3767.0,  577.0 ],
                      fromList [ 138606.0,~13478.0,~4964.0, 1441.0,~1319.0,~1482.0,  427.0, 1236.0, ~9167.0,~1918.0 ],
                      fromList [  71234.0,~41116.0, 5334.0,~4935.0,~1848.0,   66.0,  434.0,~1748.0,  3780.0, ~701.0 ],
                      fromList [ ~47645.0, 11647.0, 2166.0, 3194.0,  679.0,    0.0, ~244.0, ~419.0, ~2531.0,   48.0 ] ]

  (* Normalize angle into the range -pi <= A < +pi. *)
  fun anpm a =
    let val w = Prim.rem(a,twopi) in
      if abs w >= pic
         then if a < 0.0
                 then w + twopi
                 else w - twopi
         else w
    end

  (* The reference frame is equatorial and is with respect to the
   *    mean equator and equinox of epoch j2000. *)
  fun planetpv epoch np pv =
    (* time: julian millennia since j2000. *)
    let val t = ((sub(epoch,0) - j2000) + sub(epoch,1)) / jmillenia in
      (*  compute the mean elements. *)
      let val da  = ref (sub(sub(a,np),0) + (sub(sub(a,np),1) + sub(sub(a,np),2) * t ) * t)
          val dl  = ref (((3600.0 * sub(sub(dlm,np),0) + (sub(sub(dlm,np),1) + sub(sub(dlm,np),2) * t ) * t) * a2r))
          val de  = sub(sub(e,np),0) + (sub(sub(e,np),1) + sub(sub(e,np),2) * t ) * t
          val dp  = anpm (3600.0 * sub(sub(pi,np),0) + (sub(sub(pi,np),1) + sub(sub(pi,np),2) * t ) * t ) * a2r
          val di  = (3600.0 * sub(sub(dinc,np),0) + (sub(sub(dinc,np),1) + sub(sub(dinc,np),2) * t ) * t ) * a2r
          val doh = anpm (3600.0 * sub(sub(omega,np),0) + (sub(sub(omega,np),1) + sub(sub(omega,np),2) * t ) * t ) * a2r
              (* apply the trigonometric terms. *)
          val dmu = 0.35953620 * t in
        let fun loop k =
              if k <= 7
                 then let val arga = sub(sub(kp,np),k) * dmu
                          val argl = sub(sub(kq,np),k) * dmu in
                        da := !da + (sub(sub(ca,np),k) * System.Math.Cos(arga) + sub(sub(sa,np),k) * System.Math.Sin(arga)) * 0.0000001;
                        dl := !dl + (sub(sub(cl,np),k) * System.Math.Cos(argl) + sub(sub(sl,np),k) * System.Math.Sin(argl)) * 0.0000001;
                        loop k+1
                      end
                 else 69 in
          loop 0
        end;
        let val arga = sub(sub(kp,np),8) * dmu in
          da := !da + t * (sub(sub(ca,np),8) * System.Math.Cos(arga) + sub(sub(sa,np),8) * System.Math.Sin(arga)) * 0.0000001;
          let fun loop k =
                if k <= 9
                   then let val argl = sub(sub(kq,np),k) * dmu in
                          dl := !dl + t * (sub(sub(cl,np),k) * System.Math.Cos(argl) + sub(sub(sl,np),k) * System.Math.Sin(argl)) * 0.0000001;
                          loop k+1
                        end
                   else 69 in
            loop 8
          end
        end;
        dl := Prim.rem(!dl,twopi);
        (* iterative solution of kepler's equation to get eccentric anomaly. *)
        let val am = !dl - dp in
          let val ae = ref (am + de * System.Math.Sin(am))
              val k = ref 0 in 
            let val dae = ref ((am - !ae + de * System.Math.Sin(!ae)) / (1.0 - de * System.Math.Cos(!ae))) in
              ae := !ae + !dae;
              k := !k+1;
              while ((!k < 10) orelse (abs(!dae) >= 1e~12)) do
                (dae := (am - !ae + de * System.Math.Sin(!ae)) / (1.0 - de * System.Math.Cos(!ae));
                 ae := !ae + !dae;
                 k := !k+1);
              (* true anomaly. *)
              let val ae2 = !ae / 2.0 in
                let val at = (2.0 * System.Math.Atan2( 1.0 * System.Math.Sqrt((1.0 + de) / (1.0 - de)) * System.Math.Sin(ae2), System.Math.Cos(ae2) ))
                        (* distance (au) and speed (radians per day). *)
                    val r = !da * (1.0 - de * System.Math.Cos(!ae))
                    val v = gaussk * System.Math.Sqrt((1.0 + 1.0 / sub(amas,np)) / (!da * !da * !da))
                    val si2   = System.Math.Sin(di / 2.0) in
                  let val xq = si2 * System.Math.Cos(doh)
                      val xp = si2 * System.Math.Sin(doh)
                      val tl = at + dp in
                    let val xsw = System.Math.Sin(tl)
                        val xcw = System.Math.Cos(tl) in
                      let val xm2 = 2.0 * (xp * xcw - xq * xsw ) 
                          val xf  = !da / System.Math.Sqrt(1.0 - de * de)
                          val ci2 = System.Math.Cos(di / 2.0) in
                        let val xms = de * System.Math.Sin(dp + xsw) * xf
                            val xmc = de * System.Math.Cos(dp + xcw) * xf
                            val xpxq2 = 2.0 * xp * xq in
                          (* position (j2000 ecliptic x,y,z in au). *)
                          let val x = r * (xcw - xm2 * xp)
                              val y = r * (xsw + xm2 * xq)
                              val z = r * (0.0 - xm2 * ci2) in
                            (* rotate to equatorial. *)
                            update(sub(pv,0), 0, x);
                            update(sub(pv,0), 1, y * coseps - z * sineps);
                            update(sub(pv,0), 2, y * sineps + z * coseps);
                            (* velocity (j2000 ecliptic xdot,ydot,zdot in au/d). *)
                            let val x = v * ((~1.0 + 2.0 * xp * xp) * xms + xpxq2 * xmc)
                                val y = v * (( 1.0 - 2.0 * xq * xq ) * xmc - xpxq2 * xms)
                                val z = v * (2.0 * ci2 * (xp * xms + xq * xmc)) in
                              (* rotate to equatorial *)
                              update(sub(pv,1), 0, x);
                              update(sub(pv,1), 1, y * coseps - z * sineps);
                              update(sub(pv,1), 2, y * sineps + z * coseps)
                            end
                          end
                        end
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end

  (* Computes RA, Declination, and distance from a state vector returned by 
      planetpv. *)
  fun radecdist state rdd =
    ((* Distance *)
     update(rdd, 2, System.Math.Sqrt(  sub(sub(state,0),0) * sub(sub(state,0),0)
                                     + sub(sub(state,0),1) * sub(sub(state,0),1)
                                     + sub(sub(state,0),2) * sub(sub(state,0),2)));
     (* RA *)
     update(rdd, 0, System.Math.Atan2(sub(sub(state,0),1), sub(sub(state,0),0) * r2h));
     if sub(rdd,0) < 0.0
        then update(rdd, 0, sub(rdd,0) + 24.0)
        else ();
     (* Declination *)
     update(rdd, 1, System.Math.Asin(sub(sub(state,0),2) / sub(rdd,2) * r2d)))

  fun main (a : string option array option) = 
    let val jd = fromList [ 0.0, 0.0 ]
        val pv = fromList [ fromList [ 0.0, 0.0, 0.0 ],
                            fromList [ 0.0, 0.0, 0.0 ] ]
        val position = fromList [ 0.0, 0.0, 0.0 ] in
      let fun loop i =
            if i <= test_loops - 1
               then (update(jd, 0, j2000);
                     update(jd, 1, 0.0);
                     let fun loop n = 
                           if n <= test_length - 1
                              then (update(jd, 0, sub(jd,0) + 1.0);
                                    let fun loop p =
                                          if p <= 7
                                             then (planetpv jd p pv;
                                                   radecdist pv position;
                                                   loop p+1)
                                             else 69 in
                                      loop 0
                                    end;
                                    loop n+1)
                              else 69 in
                       loop 0
                     end;
                     loop i+1)
                else 69 in
        loop 0
      end;
      print "\n";
      print_vector jd;
      print "\n";
      print_matrix pv;
      print "\n";
      ()
    end
end
