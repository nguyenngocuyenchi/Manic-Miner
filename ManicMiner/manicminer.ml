open Engine

let manic=
 "
  ███╗░░░███╗░█████╗░███╗░░██╗██╗░█████╗░  ███╗░░░███╗██╗███╗░░██╗███████╗██████╗░
  ████╗░████║██╔══██╗████╗░██║██║██╔══██╗  ████╗░████║██║████╗░██║██╔════╝██╔══██╗
  ██╔████╔██║███████║██╔██╗██║██║██║░░╚═╝  ██╔████╔██║██║██╔██╗██║█████╗░░██████╔╝
  ██║╚██╔╝██║██╔══██║██║╚████║██║██║░░██╗  ██║╚██╔╝██║██║██║╚████║██╔══╝░░██╔══██╗
  ██║░╚═╝░██║██║░░██║██║░╚███║██║╚█████╔╝  ██║░╚═╝░██║██║██║░╚███║███████╗██║░░██║
  ╚═╝░░░░░╚═╝╚═╝░░╚═╝╚═╝░░╚══╝╚═╝░╚════╝░  ╚═╝░░░░░╚═╝╚═╝╚═╝░░╚══╝╚══════╝╚═╝░░╚═╝"
let manic_width, manic_height = get_bounds manic

(* Déclarations des éléments. *)
let personnage_left = "(☼_☼)\n╭( |╮\n ⟨ \\"
let personnage_right = "(☼_☼)\n╭| )╮\n / 〉"

let personnage_default = "(☼_☼)\n /◯\\\n / \\"
let personnage = ref personnage_default

let perso_width, perso_height = get_bounds !personnage

let game_over =
  "
  ░██████╗░░█████╗░███╗░░░███╗███████╗  ░█████╗░██╗░░░██╗███████╗██████╗░
  ██╔════╝░██╔══██╗████╗░████║██╔════╝  ██╔══██╗██║░░░██║██╔════╝██╔══██╗
  ██║░░██╗░███████║██╔████╔██║█████╗░░  ██║░░██║╚██╗░██╔╝█████╗░░██████╔╝
  ██║░░╚██╗██╔══██║██║╚██╔╝██║██╔══╝░░  ██║░░██║░╚████╔╝░██╔══╝░░██╔══██╗
  ╚██████╔╝██║░░██║██║░╚═╝░██║███████╗  ╚█████╔╝░░╚██╔╝░░███████╗██║░░██║
  ░╚═════╝░╚═╝░░╚═╝╚═╝░░░░░╚═╝╚══════╝  ░╚════╝░░░░╚═╝░░░╚══════╝╚═╝░░╚═╝"
 
 let game_complete = 
  "
  ░██████╗░░█████╗░███╗░░░███╗███████╗  ░█████╗░░█████╗░███╗░░░███╗██████╗░██╗░░░░░███████╗████████╗███████╗
  ██╔════╝░██╔══██╗████╗░████║██╔════╝  ██╔══██╗██╔══██╗████╗░████║██╔══██╗██║░░░░░██╔════╝╚══██╔══╝██╔════╝
  ██║░░██╗░███████║██╔████╔██║█████╗░░  ██║░░╚═╝██║░░██║██╔████╔██║██████╔╝██║░░░░░█████╗░░░░░██║░░░█████╗░░
  ██║░░╚██╗██╔══██║██║╚██╔╝██║██╔══╝░░  ██║░░██╗██║░░██║██║╚██╔╝██║██╔═══╝░██║░░░░░██╔══╝░░░░░██║░░░██╔══╝░░
  ╚██████╔╝██║░░██║██║░╚═╝░██║███████╗  ╚█████╔╝╚█████╔╝██║░╚═╝░██║██║░░░░░███████╗███████╗░░░██║░░░███████╗
  ░╚═════╝░╚═╝░░╚═╝╚═╝░░░░░╚═╝╚══════╝  ░╚════╝░░╚════╝░╚═╝░░░░░╚═╝╚═╝░░░░░╚══════╝╚══════╝░░░╚═╝░░░╚══════╝"
(* Déclarations des variables d'état. *)
let perso_width, perso_height = get_bounds !personnage

let init_state = (10.0, 2.0), (0.0, 0.0)

(* Fonction récursive pour construire un bloc de caractères avec largeur et hauteur donnés. *)
let rec build_block c w h =
  if h <= 0 then ""
  else if h = 1 then String.make w c
  else String.make w c ^ "\n" ^ build_block c w (h - 1)

(* Définition des blocs solides du sol et leurs positions. *)
let sol0 = (0.0, 0.0), build_block '#' (int_of_float width) 1
let sol1 = (0.0, 1.0), build_block '#' (int_of_float width) 1

(* Définition des blocs obstacles avec leurs chaînes de caractères. *)
let obstacle1 = (2.0, 2.0), build_block '#' 3 2
let obstacle2 = (0.0, 18.0), build_block '#' 10 2
let obstacle3 = (142.0, 12.0), build_block '#' 5 2
let obstacle4 = (18.0, 5.0), build_block '#' 50 2
let obstacle5 = (68.0, 7.0), build_block '#' 80 2
let obstacle6 = (0.0, 14.0), build_block '#' 14 2
let obstacle7 = (30.0, 14.0), build_block '#' 112 2
let obstacle8 = (5.0, 25.0), build_block '#' 147 2

let obstacles = [sol0; sol1; obstacle1; obstacle2; obstacle3; obstacle4; obstacle5; obstacle6; obstacle7; obstacle8]

(*MAGNIFY*)
let string_to_list s =
  let rec string_to_list_aux i l =
    if i < 0 then l
    else string_to_list_aux (i - 1) (s.[i] :: l)
  in string_to_list_aux (String.length s - 1) []
  
(**)
let rec magnify_ligne nx ny x y ipos decor line =
  match line with
  | [] -> (0.0,y +. 1.0, decor)
  | ' ' :: r -> magnify_ligne nx ny (x +. 1.0) y ipos decor r
  | '@' :: r ->
      let ipos = (x *. float_of_int nx, y *. float_of_int ny) in
      magnify_ligne nx ny (x +. 1.0) y ipos decor r
  | h :: r ->
      let solid = ((x *. float_of_int nx, y *. float_of_int ny), build_block h nx ny) in
      magnify_ligne nx ny (x +. 1.0) y ipos (solid :: decor) r

(**)
let rec magnify_aux nx ny x y ipos decor lst_s =
  match List.rev lst_s with
  | [] -> (ipos, decor)
  | h :: t ->
    let new_x, new_y, new_decor =
      match string_to_list h with
      | [] -> let new_x, new_y = (0.0,y +. 1.0) in
        new_x, new_y, decor
      | line -> magnify_ligne nx ny x y (0.0, 0.0) decor line
    in
    magnify_aux nx ny new_x new_y ipos new_decor t

(**)
let magnify nx ny s =
 magnify_aux nx ny 0.0 0.0 (0.0,0.0) [] (String.split_on_char '\n' s)

(*PROJECTILE*)
let projectile = "⁍"
let projectiles = ref []
(* Fonction pour mettre à jour des collisions entre les projectiles et les obstacles. *)
let update_projectiles () =
  projectiles :=
    List.filter (fun (p_x, p_y) ->
        not (List.exists (fun ((obs_x, obs_y), obstacle) ->
               collision ((p_x, p_y), projectile) ((obs_x, obs_y), obstacle)
             ) obstacles)
    )!projectiles;
    (* Mettre à jour les positions des projectiles. *)
  projectiles := List.map (fun (p_x, p_y) -> (p_x +. 1.0, p_y)) !projectiles

(*OBJETS*)
let objet = "🍄"
let objets = ref [(115.0, 2.0);(144.0, 14.0); (90.0, 9.0); (4.0, 20.0); (70.0, 27.0);] 
let objets_restants = ref (List.length !objets)

let update_objets (x, y) =
  objets :=
    List.filter (fun ((obj_x, obj_y)) ->
      not (collision ((x, y), !personnage) ((obj_x, obj_y), objet))
  )!objets;
  objets_restants := List.length !objets

(*ENNEMIS*)
let ennemi1=ref "(✜_✜)\n /o\\\n / \\"
let ennemi1_l="(✜_✜)\n /o\\\n / 〉"
let ennemi1_r="(✜_✜)\n /o\\\n ⟨ \\"

let ennemis1 = ref [(120.0, 2.0); (80.0, 16.0);] 
let generer_limites ennemis1 =
List.map (fun (x, _) -> ((x -. 10.0, x +. 10.0))) ennemis1
let ennemis_limites = generer_limites !ennemis1
let droite= ref false

let update_ennemis1 ennemis1 ennemis_limites droite =
  let update_ennemis_aux1 (e_x, e_y) (limit_left, limit_right) =
    if not !droite then 
      if e_x <= limit_left then (droite := true;ennemi1:=ennemi1_l;(e_x, e_y))
      else (e_x -. 1.0, e_y)
    else 
      if e_x >= limit_right then (droite := false;ennemi1:=ennemi1_r;(e_x, e_y))
      else (e_x +. 1.0, e_y)
  in ennemis1 := List.map2 update_ennemis_aux1 !ennemis1 ennemis_limites

let ennemi2 = "(⊙_☉)\n /|\\\n / \\"

let ennemis2 = ref [(25.0, 20.0);( 142.0,22.0)]
let generer_init_pos ennemis2 =
  List.map (fun (x, y) -> (x, y)) ennemis2
let ennemis_init_pos = generer_init_pos !ennemis2

let update_ennemis2 ennemis2 ennemis_init_pos =
  let update_ennemis_aux2 (e_x, e_y) (init_pos_x, init_pos_y) =
    if not (List.exists (fun ((obs_x, obs_y), obs_str) -> collision ((obs_x, obs_y), obs_str) ((e_x, e_y), ennemi2)) obstacles) then 
      (e_x, e_y -. 0.2)
    else 
      (init_pos_x, init_pos_y)
  in
  ennemis2 := List.map2 update_ennemis_aux2 !ennemis2 ennemis_init_pos

(*VIE*)
let vie=" 𓆩❣️𓆪 "
let vie_width,vie_height = get_bounds vie
let vies = ref 5
let last_collision_time = ref 0.0
let collision_pause = 2.0

let pos, _ = init_state 
let init_pos = pos 

let update_vies (x, y) ennemis ennemi =
  let temps_actuel = Unix.time () in
  if List.exists (fun (e_x, e_y) ->
    collision ((x, y), !personnage) ((e_x, e_y), ennemi)) !ennemis &&
    temps_actuel -. !last_collision_time >= collision_pause 
    then ( 
    vies := !vies - 1;
    last_collision_time := temps_actuel;
    true
    ) 
  else (false)

let rec lst_pos_vies vies =
  match vies with
  | 0 -> []
  | n -> 
    let x = (float_of_int (n - 1)) *. (vie_width +. 2.0) in
    (x, (height -. manic_height -. 2.0)) :: lst_pos_vies (n - 1)

(*TIMER*)

let timer = ref 60.0
let update_timer () = timer := max 0.0 (!timer -. 0.035)

(*GAME COMPLETE*)

let door = (80.0, 15.0), 
" __________\n|  __  __  |\n| |  ||  | |\n| |__||__| |\n|  __  __ O|\n| |  ||  | |\n| |  ||  | |\n| |__||__| |\n|__________|"

 let affiche_game_complete=((width /. 2.0 -. 35.0, height /. 2.0), game_complete)

let check_game_complete (x, y) =
  let ((d_x, d_y), door_str) = door in
  !objets_restants = 0 && collision ((d_x, d_y), door_str) ((x, y), !personnage)
  
(*GAME OVER*)

let check_game_over () =
  !vies <= 0 || !timer = 0.0

let affiche_game_over=((width /. 2.0 -. 35.0, height /. 2.0), game_over)

(* Fonction pour mettre à jour la position du personnage des entrées clavier. *)
let update ((x, y), (xincr, yincr)) key_mod =
  let incr =
  match key_mod with
  | Some (key, shift, _) ->
      (match key with
      | Char 'd' -> personnage:=personnage_right; if shift then  (5.0, 0.0) else (1.0, 0.0) 
      | Char 'q' -> personnage:=personnage_left; if shift then (-5.0, 0.0) else (-1.0, 0.0) 
      | Char 'z'-> if shift then (0.0, 5.0) else (0.0, 1.0) 
      | Char 's' -> if shift then (0.0, -5.0) else (0.0, -1.0) 
      | Char 'c'-> projectiles := (x +. perso_width /. 2.0 +. 1.0, y +. perso_height /. 2.0) :: !projectiles;
        (0.0, 0.0) 
      | _ -> (0.0, 0.0))
    | None ->personnage:=personnage_default; (0.0, 0.0)
  (* Mettre à jour les projectiles avec les obstacles. *)
  in 
  if check_game_complete(x,y) || check_game_over() then exit 0 else   
  update_objets (x,y);
  update_timer();
  update_projectiles ();
  update_ennemis1 ennemis1 ennemis_limites droite;
  update_ennemis2 ennemis2 ennemis_init_pos;
  (* Calculer la nouvelle position du personnage en tenant compte des entrées clavier et des collisions. *)
  let new_pos = if update_vies (x, y) ennemis1 !ennemi1 || update_vies (x, y) ennemis2 ennemi2 
    then (init_pos) 
  else (x +. (fst incr), y +. (snd incr)) in
  let new_pos, _, _ =
  update_physics (new_pos, !personnage) (fst incr, snd incr) [gravity; (fst incr, 0.0)] obstacles in
  (* Limiter les collisions de l'écran. *)
  let new_pos = (min (max 0.0 (fst new_pos)) (width -. 2.0), min (max 0.0 (snd new_pos)) (height -. 2.0))
  in (new_pos, incr)

(*La fonction map applique la donnee a chaque element de la liste d'objets solides
prend une paire (pos, str), ajoute v aux coordonnees. *)
let translate (v: vec) (solids: solid list) =
  List.map (fun (pos, str) -> ((fst pos +. fst v, snd pos +. snd v), str)) solids

let description_initial () =
  ((0.0, 0.0), build_block '#' 147 38)

(* Fonction pour afficher le jeu. *)
let affiche (pos, _) width height =
  let elements =
  [ ((0.0, height -. manic_height), manic);
    (pos, !personnage);
    ((0.0, 0.0), "");
    ((width -. 1.0, 0.0), "");
    ((width -. 1.0, height -. 1.0), "");
    ((0.0, height -. 1.0), "");
    door]
    (*(description_initial (), "");*)
  in 
  (* Liste des positions initiales des ennemis et mettre à jour la position des ennemis en fonction de leur direction. *)
  let dessine_vies = List.map (fun (v_x, v_y) -> ((v_x, v_y), vie)) (lst_pos_vies !vies) in
  let projectiles = List.map (fun (p_x, p_y) -> ((p_x, p_y), projectile)) !projectiles in
  let ennemis1 = List.map (fun ((e_x, e_y)) -> ((e_x, e_y), !ennemi1)) !ennemis1 in
  let ennemis2 = List.map (fun ((e_x, e_y)) -> ((e_x, e_y), ennemi2)) !ennemis2 in
  let objets = List.map (fun ((obs_x, obs_y)) -> ((obs_x, obs_y), objet)) !objets in

  (* Afficher le temps restant. *)
  let affiche_timer = [((100.0, 30.0), "⏳  restant : " ^ string_of_int (int_of_float !timer))] in
  
  (* Afficher les objets restants. *)
  let affiche_objets_restants = [((125.0, 30.0), "🍄 restants : " ^ string_of_int !objets_restants)] in
   
  (* Afficher le résultat. *)
  let endgame =
    if check_game_complete(fst pos,snd pos) then [affiche_game_complete]
    else if check_game_over() then [affiche_game_over]
    else []
  in    

  (*VECTEUR*)
  let vecteur =
    if fst pos > (width /. 2.0) then ((width /. 2.0) -. (perso_width /. 2.0) -. fst pos, 0.0) else (0.0, 0.0) in

  (* Liste des éléments à afficher. *)
  let all_elements =
    elements @ projectiles @ ennemis1 @ ennemis2  @ dessine_vies @ objets 
    @ affiche_timer @ affiche_objets_restants @ obstacles  @ endgame
  in all_elements
  
(*Main loop*)
let _ = loop init_state (fun state key_mod -> update state key_mod) (fun state -> affiche state width height)