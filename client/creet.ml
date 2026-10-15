open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix

type state = Healthy | Sick | Berserk | Mean

type creet = {
  mutable x : float;
  mutable y : float;
  mutable dx : float;
  mutable dy : float;
  mutable radius : float;
  mutable state : state;
  mutable grabbed : bool;
  mutable growth : int;
  mutable spawn_time : float;
  mutable sick_time : float;
  elt : Dom_html.divElement Js.t;
}

let all : creet list ref = ref []

let number_of_creets () = List.length !all
let time_before_spawn = ref 10000.
let contamination_rate = ref 0.02
let time_before_death = ref 30000.
let river_height = 20.
let hospital_height = 50.
let start_time = (new%js Js.date_now)##getTime |> Js.to_float

let speed_factor () =
  let elapsed =
    (((new%js Js.date_now)##getTime |> Js.to_float) -. start_time) /. 1200.
  in
  1. +. elapsed /. 80.

let create ~x ~y ~dx ~dy =
  let elt = Dom_html.createDiv Dom_html.document in
  elt##.style##.position := Js.string "absolute";
  elt##.style##.borderRadius := Js.string "50%";
  elt##.style##.background := Js.string "green";
  let c = { x; y; dx; dy; radius = 10.; state = Healthy; elt; grabbed = false; growth = 0; spawn_time = (new%js Js.date_now)##getTime |> Js.to_float; sick_time = 0. } in
  all := c :: !all;
  c

let render c =
  let px f = Js.string (Printf.sprintf "%.1fpx" f) in
  c.elt##.style##.left := px (c.x -. c.radius);
  c.elt##.style##.top := px (c.y -. c.radius);
  c.elt##.style##.width := px (2. *. c.radius);
  c.elt##.style##.height := px (2. *. c.radius);
  c.elt##.style##.transition := Js.string "box-shadow 220ms ease, transform 220ms ease";
  c.elt##.style##.border := Js.string "1.5px solid rgba(255, 255, 255, 0.8)";
  c.elt##.style##.boxShadow :=
    Js.string "0 2px 7px rgba(24, 38, 46, 0.28), inset 1px 1px 2px rgba(255, 255, 255, 0.45)";
  c.elt##.style##.transition := Js.string "background 220ms ease, box-shadow 220ms ease, transform 220ms ease";
  c.elt##.style##.zIndex := Js.string "2";
  c.elt##.style##.cursor := Js.string (if c.grabbed then "grabbing" else "grab")

let render_river () =
  let river = Dom_html.createDiv Dom_html.document in
  river##.style##.position := Js.string "absolute";
  river##.style##.left := Js.string "0px";
  river##.style##.top := Js.string "0px";
  river##.style##.width := Js.string "100%";
  river##.style##.height := Js.string (Printf.sprintf "%.1fpx" river_height);
  river##.style##.background :=
    Js.string "linear-gradient(180deg, #4a9ed0 0%, #287db7 48%, #1d679d 100%)";
  river##.style##.boxShadow := Js.string "0 2px 7px rgba(24, 91, 139, 0.28)";
  river##.style##.borderBottom := Js.string "1px solid rgba(255, 255, 255, 0.45)";
  river##.style##.pointerEvents := Js.string "none";
  river##.style##.zIndex := Js.string "1";
  Dom.appendChild Dom_html.document##.body river

let render_hospital () =
  let hospital = Dom_html.createDiv Dom_html.document in
  hospital##.style##.position := Js.string "absolute";
  hospital##.style##.left := Js.string "0px";
  hospital##.style##.top := Js.string (Printf.sprintf "%.1fpx" (float_of_int Dom_html.window##.innerHeight -. hospital_height));
  hospital##.style##.width := Js.string "100%";
  hospital##.style##.height := Js.string (Printf.sprintf "%.1fpx" hospital_height);
  hospital##.style##.background :=
    Js.string "repeating-linear-gradient(135deg, rgba(255, 255, 255, 0.72) 0px, rgba(255, 255, 255, 0.72) 2px, rgba(226, 235, 232, 0.72) 2px, rgba(226, 235, 232, 0.72) 8px)";
  hospital##.style##.borderTop := Js.string "2px solid rgba(72, 128, 108, 0.55)";
  hospital##.style##.boxShadow := Js.string "0 -3px 12px rgba(57, 91, 82, 0.13)";
  hospital##.style##.pointerEvents := Js.string "none";
  hospital##.style##.zIndex := Js.string "1";
  Dom.appendChild Dom_html.document##.body hospital

let bounce c ~w ~h =
  if c.x -. c.radius < 0. then begin
    c.x <- c.radius;
    c.dx <- abs_float c.dx
  end;
  if c.x +. c.radius > w then begin
    c.x <- w -. c.radius;
    c.dx <- -. (abs_float c.dx)
  end;
  if c.y -. c.radius < 0. then begin
    c.y <- c.radius;
    c.dy <- abs_float c.dy
  end;
  if c.y +. c.radius > h then begin
    c.y <- h -. c.radius;
    c.dy <- -. (abs_float c.dy)
  end

let get_sick c =
    if c.state = Healthy && not c.grabbed then begin
        c.state <- Sick;
        c.elt##.style##.background := Js.string "red";
        c.dx <- c.dx *. 0.85;
        c.dy <- c.dy *. 0.85;
        if Random.float 1. < 0.1 then begin
            c.state <- Berserk;
            c.elt##.style##.background := Js.string "orange";
        end
        else if Random.float 1. < 0.1 then begin
            c.state <- Mean;
            c.elt##.style##.background := Js.string "purple";
            c.radius <- c.radius *. 0.85
        end;
        c.sick_time <- (new%js Js.date_now)##getTime |> Js.to_float
    end

let touch_river c =
  if c.y -. c.radius < river_height then get_sick c

let contaminate c =
  if c.state <> Healthy then
    List.iter (fun other ->
      if other != c
         && other.state = Healthy
         && abs_float (c.x -. other.x) < c.radius +. other.radius
         && abs_float (c.y -. other.y) < c.radius +. other.radius
         && Random.float 1. < !contamination_rate
      then get_sick other
    ) !all

let in_hospital c =
  c.y +. c.radius > float_of_int Dom_html.window##.innerHeight -. hospital_height

let heal c =
  if c.state <> Healthy then begin
    c.state <- Healthy;
    c.growth <- 0;
    c.radius <- 10.;
    c.elt##.style##.background := Js.string "green"
  end

let dist2 a b =
  let dx = b.x -. a.x in
  let dy = b.y -. a.y in
  dx *. dx +. dy *. dy

let nearest_healthy c =
  List.fold_left (fun best other ->
    if other == c || other.state <> Healthy || other.grabbed then best
    else match best with
      | None -> Some other
      | Some b -> if dist2 c other < dist2 c b then Some other else best
  ) None !all

let move c =
  let w = float_of_int Dom_html.window##.innerWidth in
  let h = float_of_int Dom_html.window##.innerHeight in
  if c.state = Mean then begin
    match nearest_healthy c with
    | None -> ()
    | Some target ->
      let vx = target.x -. c.x in
      let vy = target.y -. c.y in
      let d = sqrt (vx *. vx +. vy *. vy) in
      let speed = sqrt (c.dx *. c.dx +. c.dy *. c.dy) in
      if d > 0. then begin
        c.dx <- vx /. d *. speed;
        c.dy <- vy /. d *. speed
      end
  end;
  if not c.grabbed then begin
    c.x <- c.x +. c.dx *. speed_factor ();
    c.y <- c.y +. c.dy *. speed_factor ();
    bounce c ~w ~h;
    render c;
    contaminate c;
    touch_river c
  end

let check_end_game () =
    let all_sick = List.for_all(fun c -> c.state = Sick || c.state = Berserk || c.state = Mean) !all in
    if all_sick then begin
        let end_game_div = Dom_html.createDiv Dom_html.document in
        end_game_div##.style##.position := Js.string "absolute";
        end_game_div##.style##.left := Js.string "50%";
        end_game_div##.style##.top := Js.string "50%";
        end_game_div##.style##.transform := Js.string "translate(-50%, -50%)";
        end_game_div##.style##.padding := Js.string "20px";
        end_game_div##.style##.background := Js.string "rgba(0, 0, 0, 0.8)";
        end_game_div##.style##.color := Js.string "white";
        end_game_div##.style##.fontSize := Js.string "24px";
        end_game_div##.style##.borderRadius := Js.string "10px";
        end_game_div##.innerHTML := Js.string "All creets are sick or dying! You lose!";
        Dom.appendChild Dom_html.document##.body end_game_div;
        true
    end
    else false

let check_death c =
    let now = (new%js Js.date_now)##getTime |> Js.to_float in
    if c.state <> Healthy && not c.grabbed then begin
        if now -. c.sick_time >= !time_before_death then begin
            all := List.filter (fun other -> other != c) !all;
            Dom.removeChild Dom_html.document##.body c.elt;
        end
    end

let reproduction c =
    let now = (new%js Js.date_now)##getTime |> Js.to_float in
    if c.state = Healthy && not c.grabbed 
        && now -. c.spawn_time >= !time_before_spawn && number_of_creets () < 50 then begin
        let w = float_of_int Dom_html.window##.innerWidth in
        let h = float_of_int Dom_html.window##.innerHeight in
        let x = Random.float w in
        let y = Random.float h in
        let speed = 1. +. Random.float 3. in
        let angle = Random.float (2. *. Float.pi) in
        let child =
            create ~x ~y
                ~dx:(speed *. cos angle)
                ~dy:(speed *. sin angle)
        in
        child.spawn_time <- now;
        c.spawn_time <- now;
        Dom.appendChild Dom_html.document##.body child.elt;
        Some child
    end
    else None


let rec live c =
  Lwt_js.sleep 0.016 >>= fun () ->
  if check_end_game () then Lwt.return ()
  else begin
    move c;
    check_death c;
    (match reproduction c with
     | Some child -> Lwt.async (fun () -> live child)
     | None -> ());
    if c.state = Berserk && c.growth < 100 then begin
      c.growth <- c.growth + 1;
      c.radius <- 10. *. (1. +. 3. *. float_of_int c.growth /. 100.)
    end;
    live c
  end