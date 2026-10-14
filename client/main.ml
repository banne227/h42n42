open Js_of_ocaml
open Js_of_ocaml_lwt

let held = ref (None : Creet.creet option)

let setup_mouse () =
  Lwt.async (fun () ->
    Lwt_js_events.mousedowns Dom_html.document (fun ev _ ->
      let mx = Js.to_float ev##.clientX in
      let my = Js.to_float ev##.clientY in
      let found =
        List.find_opt (fun (c : Creet.creet) ->
          let dx = mx -. c.Creet.x in
          let dy = my -. c.Creet.y in
          dx *. dx +. dy *. dy <= c.Creet.radius *. c.Creet.radius
        ) !Creet.all
      in
      (match found with
       | Some c ->
         c.Creet.grabbed <- true;
         held := Some c
       | None -> ());
      Lwt.return ()));

  Lwt.async (fun () ->
    Lwt_js_events.mousemoves Dom_html.document (fun ev _ ->
      let mx = Js.to_float ev##.clientX in
      let my = Js.to_float ev##.clientY in
      (match !held with
       | Some c ->
         c.Creet.x <- mx;
         c.Creet.y <- my;
         Creet.render c
       | None -> ());
      Lwt.return ()));

  Lwt.async (fun () ->
    Lwt_js_events.mouseups Dom_html.document (fun _ _ ->
      (match !held with
       | Some c ->
         c.Creet.grabbed <- false;
         if Creet.in_hospital c && c.Creet.state <> Creet.Healthy then begin
           Creet.heal c;
         end;
         held := None
       | None -> ());
      Lwt.return ()))

let () =
  setup_mouse ();
  Random.self_init ();
  Creet.render_river ();
  Creet.render_hospital ();
  Dom_html.window##.onload := Dom_html.handler (fun _ -> Js._true);
  let w = float_of_int Dom_html.window##.innerWidth in
  let h = float_of_int Dom_html.window##.innerHeight in

  let nb_creets = 10 in
  for _ = 1 to nb_creets do
    let x = Random.float w in
    let y = Random.float h in
    let speed = 1. +. Random.float 3. in
    let angle = Random.float (2. *. Float.pi) in
    let c =
      Creet.create ~x ~y
        ~dx:(speed *. cos angle)
        ~dy:(speed *. sin angle)
    in
    Dom.appendChild Dom_html.document##.body c.Creet.elt;
    Creet.render c;
    Lwt.async (fun () -> Creet.live c)
  done