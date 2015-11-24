(* Public testcase 5 : sorting *) 

let 
  val data = (1,(400,(29,1)))
  val bg = fn a => fn b => 
    let rec comp = fn a => fn b => fn delta =>
          if (a+delta) = b then false
          else if (a-delta) = b then true
          else comp a b (delta+1)
    in
        if a = b then false
        else comp a b 1
    end 
  val sort2 = fn x => 
    let val a = x.1 
        val b = x.2
    in
        if (bg a b) then (a,b) else (b,a)
    end
  val sort3 = fn x => 
    let val a = x.1 
        val b = sort2 (x.2)
        val c = b.1
        val d = b.2
    in
        if (bg d a) then (c,(d,a)) 	
        else if (bg c a) then (c,(a,d))
        else (a,b)
    end
  val sort4 = fn x =>
    let val a = x.1
        val b = sort3 (x.2)
        val c = b.1
    in
        if (bg c a) then (c, sort3 (a,b.2))
        else (a,b) 
    end 
  val print2 = fn x => 				
    let val a = x.1 
        val b = x.2
    in
        write a; write b
    end
  val print3 = fn x => 
      let val p = x.1
          val q = x.2 
      in 
          write (x.1); print2 (x.2)
      end  
  val print4 = fn x =>
      let val p = x.1
          val q = x.2
      in write p; print3 (x.2) end
in
  print4 data; write "sort";
  print4 (sort4 data) 
end 

