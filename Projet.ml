Random.self_init ();;

let rec iterate : (int * ('a -> 'a) * 'a) -> 'a =
  fun (count, f, initial_value) ->
    if count <= 0
    then initial_value
    else iterate (count - 1, f, f initial_value)
;;

let repeat_string = fun (x, y) -> iterate(y, (fun s -> s ^ x), x);;

type classe = Archer | Warrior | Wizard;;
type gender = Male | Female;;
type obj = Nothing | Sponges | Chickens | Coins | Healing_Potion | Magic_Potion;;
type item = {quantity: int; name: obj};;
type inventory = item list;;
type character = {name: string; classe: classe; gender: gender; hp: int; max_hp: int; mp: int; max_mp: int; xp: int; level: int; bag: inventory};;
type race = Golem | Mosquito | Boar;;
type monster = {typee: race; hp: int; xp: int; loot: item};;
type sleep_event = Character of character | Monster of monster | Eat of (bool * character) | Stats of string;; 

let classe_gender = fun r -> match r with
  | {classe=Archer} -> if (r.gender = Male) then "Archer" else "Archeress"
  | {classe=Warrior} -> if (r.gender = Male) then "Warrior" else "Warrioress"
  | {classe=Wizard} -> if (r.gender = Male) then "Wizard" else "Magicienne";;

let header = fun (p: character) -> if (p.level < 10) then "+" ^ repeat_string("-", String.length(p.name) + String.length(classe_gender p) + String.length("level 10") + 4) ^ "+"
  else "+" ^ repeat_string("-", String.length(p.name) + String.length(classe_gender p) + String.length("level 10") + 5) ^ "+";;
  
let get_name = fun () -> let () = print_string "\n\nPlease enter your name: "
  in let i = read_line()
  in i;;

let get_gender = fun () -> let () = print_string "\n\nChoose your character's gender: \n\n1. Male\n2. Female\n"
  in let i = read_int()
  in if(i = 1) then Male
  else Female;;

let get_classe = fun () -> let () = print_string "\n\nPlease choose your classe from the following: \n\n1. Warrior\n2. Archer\n3. Wizard\n"
  in let i = read_int()
  in 
  if(i = 1) then Warrior
  else if (i = 2) then Archer 
  else Wizard;;

let bag: inventory = [{quantity=0; name=Chickens}; {quantity=0; name=Coins}; {quantity=0; name=Sponges};
                      {quantity=0; name=Healing_Potion}; {quantity=0; name=Magic_Potion}];;
let player = {name=get_name(); classe=get_classe(); gender=get_gender(); hp=20; max_hp=20; mp=0; max_mp=40; xp=0; level=1; bag=bag};;

let manage_mana = fun (p: character) -> 
  if (p.classe = Wizard) then {name=p.name; classe=p.classe; gender=p.gender; hp=20; max_hp=20; mp=40; max_mp=40; xp=0; level=1; bag=bag}
  else {name=p.name; classe=p.classe; gender=p.gender; hp=20; max_hp=20; mp=0; max_mp=0; xp=0; level=1; bag=bag};;

let translate_string = fun (o: item) -> match o with
  | {quantity=int; name=Coins} -> if (o.quantity > 1) then string_of_int(o.quantity) ^ " coins" else string_of_int(o.quantity) ^ " coin"
  | {quantity=int; name=Chickens} -> if (o.quantity > 1) then string_of_int(o.quantity) ^ " chickens" else string_of_int(o.quantity) ^ " chicken"
  | {quantity=int; name=Sponges} -> if (o.quantity > 1) then string_of_int(o.quantity) ^ " sponges" else string_of_int(o.quantity) ^ " sponge"
  | {quantity=int; name=Healing_Potion} -> if (o.quantity > 1) then string_of_int(o.quantity) ^ " Healing Potions" else string_of_int(o.quantity) ^ " Healing Potion"
  | {quantity=int; name=Magic_Potion} -> if (o.quantity > 1) then string_of_int(o.quantity) ^ " Magic Potions" else string_of_int(o.quantity) ^ " Magic Potion"
  | {quantity=int; name=_} -> "";;  

let rec show_inventory = fun (s: inventory) (p: character) -> match s with
  | [] -> "" 
  | h::t -> if (h.name = Chickens) then "|  " ^ translate_string h ^ repeat_string(" ", String.length(header p) - String.length(translate_string h) - 5) ^ "|" ^ "\n" ^ show_inventory t p
      else if(h.name = Coins) then "|  " ^ translate_string h ^ repeat_string(" ", String.length(header p) - String.length(translate_string h) - 5) ^ "|" ^ "\n" ^ show_inventory t p
      else if (h.name = Healing_Potion) then "|  " ^ translate_string h ^ repeat_string(" ", String.length(header p) - String.length(translate_string h) - 5) ^ "|" ^ "\n" ^ show_inventory t p
      else if (h.name = Magic_Potion) then "|  " ^ translate_string h ^ repeat_string(" ", String.length(header p) - String.length(translate_string h) - 5) ^ "|" ^ "\n" ^ show_inventory t p
      else "|  " ^ translate_string h ^ repeat_string(" ", String.length(header p) - String.length(translate_string h) - 5) ^ "|" ^ "\n" ^ show_inventory t p;;

let show_stats = fun (p: character) ->
  if (p.gender = Male && p.classe = Wizard) then 
    "\n" ^ header p ^ "\n| " ^ p.name ^ " | " ^ classe_gender p ^ " level " ^ string_of_int(p.level) ^ " |\n" ^
    header p ^ "\n| Health Points |" ^ repeat_string(" ", String.length(header p) - String.length("| Health Points |") - 8) ^ string_of_int(p.hp) ^ "/" ^ string_of_int(p.max_hp) ^ " |\n" ^
    header p ^ "\n| Magic Points |" ^ repeat_string(" ", String.length(header p) - String.length("| Magic Points |") - 8) ^ string_of_int(p.mp) ^ "/" ^ string_of_int(p.max_mp) ^ " |\n" ^
    header p ^ "\n| Experience    |" ^ repeat_string(" ", String.length(header p) - String.length("| Health Points |") - 4) ^ string_of_int(p.xp) ^ " |\n" ^
    header p ^ "\n| Bag" ^ repeat_string(" ", String.length(header p) - String.length("| Bag") - 2) ^ "|\n" ^ show_inventory p.bag p ^ header p
  else if (p.gender = Female && p.classe = Wizard) then 
    "\n" ^ header p ^ "\n| " ^ p.name ^ " | " ^ classe_gender p ^ " level " ^ string_of_int(p.level) ^ "  |\n" ^
    header p ^ "\n| Health Points |" ^ repeat_string(" ", String.length(header p) - String.length("| Health Points |") - 8) ^ string_of_int(p.hp) ^ "/" ^ string_of_int(p.max_hp) ^ " |\n" ^
    header p ^ "\n| Magic Points |" ^ repeat_string(" ", String.length(header p) - String.length("| Magic Points |") - 8) ^ string_of_int(p.mp) ^ "/" ^ string_of_int(p.max_mp) ^ " |\n" ^
    header p ^ "\n| Experience    |" ^ repeat_string(" ", String.length(header p) - String.length("| Health Points |") - 4) ^ string_of_int(p.xp) ^ " |\n" ^
    header p ^ "\n| Bag" ^ repeat_string(" ", String.length(header p) - String.length("| Bag") - 2) ^ "|\n" ^ show_inventory p.bag p ^ header p ^ "\n"
  else if (p.gender = Male && p.classe != Wizard) then
    "\n" ^ header p ^ "\n| " ^ p.name ^ " | " ^ classe_gender p ^ " level " ^ string_of_int(p.level) ^ " |\n" ^
    header p ^ "\n| Health Points |" ^ repeat_string(" ", String.length(header p) - String.length("| Health Points |") - 8) ^ string_of_int(p.hp) ^ "/" ^ string_of_int(p.max_hp) ^ " |\n" ^
    header p ^ "\n| Experience    |" ^ repeat_string(" ", String.length(header p) - String.length("| Health Points |") - 4) ^ string_of_int(p.xp) ^ " |\n" ^
    header p ^ "\n| Bag" ^ repeat_string(" ", String.length(header p) - String.length("| Bag") - 2) ^ "|\n" ^ show_inventory p.bag p ^ header p
  else "\n" ^ header p ^ "\n| " ^ p.name ^ " | " ^ classe_gender p ^ " level " ^ string_of_int(p.level) ^ "  |\n" ^
       header p ^ "\n| Health Points |" ^ repeat_string(" ", String.length(header p) - String.length("| Health Points |") - 8) ^ string_of_int(p.hp) ^ "/" ^ string_of_int(p.max_hp) ^ " |\n" ^
       header p ^ "\n| Experience    |" ^ repeat_string(" ", String.length(header p) - String.length("| Health Points |") - 4) ^ string_of_int(p.xp) ^ " |\n" ^
       header p ^ "\n| Bag" ^ repeat_string(" ", String.length(header p) - String.length("| Bag") - 2) ^ "|\n" ^ show_inventory p.bag p ^ header p ^ "\n";;

       

let generate_loot = fun () -> match Random.int(6) with
  | 0 -> {quantity=Random.int(10)+1; name=Coins}
  | 1 -> {quantity=Random.int(5)+1; name=Chickens}
  | 2 -> {quantity=Random.int(10)+1; name=Sponges}
  | 3 -> {quantity=0; name=Nothing}
  | 4 -> {quantity=Random.int(2)+1; name=Healing_Potion}
  | 5 -> {quantity=Random.int(2)+1; name=Magic_Potion}
  | _ -> {quantity=0; name=Nothing};;

let generate = fun () -> match Random.int(3) with 
  | 0 -> {typee=Golem; hp=25+Random.int(6)+1; xp=8; loot=generate_loot()}
  | 1 -> let mosquito_count = (Random.int(8)+3) in
         {typee=Mosquito; hp=2+mosquito_count; xp=2; loot={quantity=0; name=Nothing}}
  | 2 -> {typee=Boar; hp=10+Random.int(4)+1; xp=4; loot=generate_loot()}
  | _ -> {typee=Boar; hp=10; xp=4; loot=generate_loot()};; 

(* ------------------------------------------------------------------------------ SPELL MANAGEMENT ------------------------------------------------------------------------------------- *)

let rec choose_spell = fun (p: character) -> let () = print_string "\n\nWhich spell do you want to cast?\n\n(B) Fireball\n\n(I) Ice Spike\n\n(F) Firestorm\n\n"
  in let i = read_line () 
  in
  if (i="B") then 
    if (p.mp < 10) then let () = print_string "Not enough mana" in choose_spell p 
    else i
  else if (i="I") then 
    if (p.mp < 5) then let () = print_string "Not enough mana" in choose_spell p 
    else i
  else if (i="F") then 
    if (p.mp < 8) then let () = print_string "Not enough mana" in choose_spell p 
    else i
  else choose_spell p;;

let rec lose_mp = fun (p: character) x -> 
  if (x="B") then {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp-10; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
  else if (x="I") then {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp-5; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
  else if (x="F") then {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp-8; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
  else lose_mp p x;;

let rec wizard_attack = fun (p: character) (m: monster) ->
  if (m.hp > 0 && p.hp > 0) then let () = print_string "\n\nWhat do you do?\n\n(H) Hand strike\n\n(C) Cast a spell\n\n" in 
    let i = read_line() 
    in
    if(i="H") then p 
    else if (i="C") then 
      if (p.mp < 5) then let () = print_string "\n\nYou don't have enough mana to cast a spell\n\n" in wizard_attack p m 
      else lose_mp p (choose_spell p)
    else wizard_attack p m
  else p;;

let rec cast_spell = fun (p: character) (m: monster) x -> 
  if (p.hp > 0 && m.hp > 0) then 
    if(x="B") then
      if((Random.int(20)+1) <= if (2+p.level*1) < 20 then 10+p.level*1 else 20) then let () = print_string "\n\nYou cast a fireball and deal 12 damage.\n\n" in {typee=m.typee; hp=m.hp-12; xp=m.xp; loot=m.loot} 
      else let () = print_string "\n\nYou counterattack, but you miss the target.\n\n" in {typee=m.typee; hp=m.hp; xp=m.xp; loot=m.loot} 
    else if(x="I") then
      if ((Random.int(20)+1) <= if (6+p.level*1) < 20 then 10+p.level*1 else 20) then let () = print_string "\n\nYou cast an ice spike and deal 7 damage.\n\n" in {typee=m.typee; hp=m.hp-7; xp=m.xp; loot=m.loot} 
      else let () = print_string "\n\nYou counterattack, but you miss the target.\n\n" in {typee=m.typee; hp=m.hp; xp=m.xp; loot=m.loot} 
    else if(x="F") then
      if((Random.int(20)+1) <= if (4+p.level*1) < 20 then 10+p.level*1 else 20) then let () = print_string "\n\nYou cast a firestorm and deal 9 damage.\n\n" in {typee=m.typee; hp=m.hp-9; xp=m.xp; loot=m.loot} 
      else let () = print_string "\n\nYou counterattack, but you miss the target.\n\n" in {typee=m.typee; hp=m.hp; xp=m.xp; loot=m.loot} 
    else cast_spell p m x
  else {typee=m.typee; hp=m.hp; xp=m.xp; loot=m.loot};;

(* ------------------------------------------------------------------------------ REGENERATION -------------------------------------------------------------------------------------- *)

let rec filter_quantity_to_eat = fun s: inventory -> match s with
  | [] -> s
  | h::t -> if (h.name=Chickens && h.quantity>0) then h::t else filter_quantity_to_eat t;;

let rec filter_eat = fun s: inventory -> match s with
  | [] -> s
  | h::t -> if (h.name=Chickens && h.quantity>0) then {quantity=h.quantity-1; name=h.name}::filter_eat t else h::filter_eat t;;

let rec filter_quantity_heal = fun s: inventory -> match s with
  | [] -> s
  | h::t -> if (h.name=Healing_Potion && h.quantity>0) then h::t else filter_quantity_heal t;;

let rec filter_quantity_magic = fun s: inventory -> match s with
  | [] -> s
  | h::t -> if (h.name=Magic_Potion && h.quantity>0) then h::t else filter_quantity_magic t;;

let rec filter_drink_heal = fun s: inventory -> match s with
  | [] -> s
  | h::t -> if (h.name=Healing_Potion && h.quantity>0) then {quantity=h.quantity-1; name=h.name}::filter_drink_heal t else h::filter_drink_heal t;;

let rec filter_drink_magic = fun s: inventory -> match s with
  | [] -> s
  | h::t -> if (h.name=Magic_Potion && h.quantity>0) then {quantity=h.quantity-1; name=h.name}::filter_drink_magic t else h::filter_drink_magic t;;

let rec drink = fun (p: character) -> let () = print_string "\n\nChoose if you want to drink a health or magic potion:\n\n1. (H) Healing Potion, restores up to 10 health points\n2. (M) Magic, restores up to 20 magic points\n"
  in let i = read_line() in
  if(i="H") then
    if (p.hp==20) then let () = print_string "\n\nYou cannot drink.\n\n" in (false,p) 
  
    else if ((p.hp<=19) && (p.hp>15) && (filter_quantity_heal p.bag!=[])) then let () = print_string ("\n\nYou drank 1 healing potion and gained "^string_of_int(20-p.hp)^" health points.\n\n") in (true,{name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+(20-p.hp); max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=filter_drink_heal p.bag})
  
    else if ((p.hp<=19) && (p.hp>15) && (filter_quantity_heal p.bag==[])) then let () = print_string "\n\nYou cannot drink.\n\n" in (false,{name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag})
  
    else if ((p.hp<=15) && (filter_quantity_heal p.bag!=[]))  then let () = print_string "\n\nYou drank 1 healing potion and gained 5 health points.\n\n" in (true,{name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+5; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=filter_drink_heal p.bag})
  
    else let () = print_string "\n\nYou cannot drink.\n\n" in (false,{name=p.name; classe=p.classe; gender=p.gender; hp=p.max_hp; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag})
  
  else if (i="M") then 
    if (p.classe=Wizard) then
    
      if (p.mp==40) then let () = print_string "\n\nYou cannot drink.\n\n" in (false,p) 
  
      else if ((p.mp<=39) && (p.mp>30) && (filter_quantity_magic p.bag!=[])) then let () = print_string ("\n\nYou drank 1 magic potion and gained "^string_of_int(40-p.mp)^" magic points.\n\n") in (true,{name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp+(40-p.mp); max_mp=p.max_mp; xp=p.xp; level=p.level; bag=filter_drink_magic p.bag})
  
      else if ((p.mp<=39) && (p.mp>30) && (filter_quantity_magic p.bag==[])) then let () = print_string "\n\nYou cannot drink.\n\n" in (false,{name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag})
  
      else if ((p.mp<=30) && (filter_quantity_magic p.bag!=[]))  then let () = print_string "\n\nYou drank 1 magic potion and gained 10 magic points.\n\n" in (true,{name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp+10; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=filter_drink_magic p.bag}) 
                                                                                                                                                                
      else let () = print_string "\n\nYou cannot drink.\n\n" in (false,{name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag})
    
    else let ()=print_string "\n\nYou don't have mana" in drink p
      
  else drink p;;


let eat = fun (p:character) ->

  if (p.hp = 20) then let () = print_string "You cannot eat.\n\n" in (false, p)
  
  else if (p.hp = 19 && (filter_quantity_to_eat p.bag != [])) then let () = print_string "You ate 1 chicken and gained 2 hit points.\n\n" in (true, {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+1; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=filter_eat p.bag})
  
  else if (p.hp = 19 && (filter_quantity_to_eat p.bag == [])) then let () = print_string "You cannot eat.\n\n" in (false, {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag})
  
  else if (p.hp < 19 && (filter_quantity_to_eat p.bag != [])) then let () = print_string "You ate 1 chicken and gained 2 hit points.\n\n" in (true, {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+2; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=filter_eat p.bag})
  
  else let () = print_string "You cannot eat.\n\n" in (false, {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag});;

let rest = fun (p:character) ->
  
  if (p.hp = 20) then 
    if (p.mp = 40) then Character p
    else if ((p.mp <= 39) && (p.mp > 32)) then Character {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp+(40-p.mp); max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
    else Character {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp+8; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
  
  else if (p.hp = 19) then 
    if (p.mp = 40) then Character p
    else if ((p.mp <= 39) && (p.mp > 32)) then Character {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+1; max_hp=p.max_hp; mp=p.mp+(40-p.mp); max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
    else Character {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+1; max_hp=p.max_hp; mp=p.mp+8; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
  
  else if (p.hp = 18) then
    if (p.mp = 40) then Character p
    else if ((p.mp <= 39) && (p.mp > 32)) then Character {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+2; max_hp=p.max_hp; mp=p.mp+(40-p.mp); max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
    else Character {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+2; max_hp=p.max_hp; mp=p.mp+8; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
  
  else if (p.hp = 17) then 
    if (p.mp = 40) then Character p
    else if ((p.mp <= 39) && (p.mp > 32)) then Character {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+3; max_hp=p.max_hp; mp=p.mp+(40-p.mp); max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
    else Character {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+3; max_hp=p.max_hp; mp=p.mp+8; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
  
  else 
  if (p.mp = 40) then Character p 
  else if ((p.mp <= 39) && (p.mp > 32)) then Character {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+4; max_hp=p.max_hp; mp=p.mp+(40-p.mp); max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
  else Character {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp+4; max_hp=p.max_hp; mp=p.mp+8; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag};;

exception YouAreDead
  
let sleep = fun (p:character) ->
  if ((Random.int(20)+1) <= 19) 
  then let () = print_string "\nYou set up your camp and quickly fall asleep.\nYou wake up the next morning and gain 4 hit points and 8 mana points.\n\n" in rest p
  
  else if (Monster (generate()) == Monster {typee=Golem; hp=(generate()).hp; xp=(generate()).xp; loot=(generate()).loot}) 
  then let () = print_string "\nYou set up your camp and quickly fall asleep.\n\nDuring your sleep, a golem appears and smashes your skull.\n\nYOU ARE DEAD!\n\n" in Monster (generate())
  
  else if (Monster (generate()) == Monster {typee=Mosquito; hp=(generate()).hp; xp=(generate()).xp; loot=(generate()).loot})
  then let () = print_string "\nYou set up your camp and quickly fall asleep.\n\nDuring your sleep, a swarm of mosquitoes drains you of your blood.\n\nYOU ARE DEAD!\n\n" in Monster (generate())
  
  else let () = print_string "\nYou set up your camp and quickly fall asleep.\n\nDuring your sleep, a wild boar sits on you.\n\nYOU ARE DEAD!\n\n" in Monster (generate());;



(*----------------------------------------------------------------------------------FIGHT-----------------------------------------------------------------------------------------------*)


let rec loot_bag = fun (l: inventory) (o: item) -> 
  match l with
  | [] -> l
  | h::t -> 
    if (h.name = o.name) then 
      {quantity = h.quantity + o.quantity; name = h.name} :: t 
    else 
      h :: loot_bag t o;;


let hit = fun (p: character) (m: monster) ->
  if (p.classe = Warrior && p.hp > 0 && m.hp > 0) then 
    if ((Random.int(20) + 1) <= (if (6 + p.level * 1) < 20 then 6 + p.level * 1 else 20)) then 
      let () = print_string "You hit and inflict 10 points of damage.\n\n" in 
      {typee = m.typee; hp = m.hp - 10; xp = m.xp; loot = m.loot}
    else 
      let () = print_string "You retaliate, but you miss the target.\n\n" in 
      {typee = m.typee; hp = m.hp; xp = m.xp; loot = m.loot}
  
  else if (p.classe = Archer && p.hp > 0 && m.hp > 0) then 
    if ((Random.int(20) + 1) <= (if (14 + p.level * 1) < 20 then 14 + p.level * 1 else 20)) then 
      let () = print_string "You hit and inflict 4 points of damage.\n\n" in 
      {typee = m.typee; hp = m.hp - 4; xp = m.xp; loot = m.loot} 
    else 
      let () = print_string "You retaliate, but you miss the target.\n\n" in 
      {typee = m.typee; hp = m.hp; xp = m.xp; loot = m.loot}
  
  else if (p.classe = Wizard && p.hp > 0 && m.hp > 0) then 
    if ((Random.int(20) + 1) <= (if (10 + p.level * 1) < 20 then 10 + p.level * 1 else 20)) then 
      let () = print_string "You hit and inflict 5 points of damage.\n\n" in 
      {typee = m.typee; hp = m.hp - 5; xp = m.xp; loot = m.loot} 
    else 
      let () = print_string "You retaliate, but you miss the target.\n\n" in 
      {typee = m.typee; hp = m.hp; xp = m.xp; loot = m.loot}
  
  else 
    {typee = m.typee; hp = m.hp; xp = m.xp; loot = m.loot};;


let monster_hit = fun (p: character) (m: monster) ->
  if (m.typee = Golem && m.hp > 0 && p.hp > 0) then 
    let () = print_string "The golem raises its fist and delivers a powerful blow, you lose 4 hit points.\n\n" in 
    {name = p.name; classe = p.classe; gender = p.gender; hp = p.hp - 4; max_hp = p.max_hp; 
     mp = p.mp; max_mp = p.max_mp; xp = p.xp; level = p.level; bag = p.bag}
  
  else if (m.typee = Boar && m.hp > 0 && p.hp > 0) then 
    let () = print_string "The boar charges and knocks you down, you lose 2 hit points.\n\n" in 
    {name = p.name; classe = p.classe; gender = p.gender; hp = p.hp - 2; max_hp = p.max_hp; 
     mp = p.mp; max_mp = p.max_mp; xp = p.xp; level = p.level; bag = p.bag}
  
  else if (m.typee = Mosquito && m.hp > 0 && p.hp > 0) then 
    let () = print_string ("The swarm of mosquitoes hits the mark, you lose " ^ 
                            string_of_int((int_of_float((1. /. 2.) *. 
                                          (float_of_int((m.hp - 2)))) + 1)) ^ " hit points.\n\n") in 
    {name = p.name; classe = p.classe; gender = p.gender; 
     hp = p.hp - (int_of_float((1. /. 2.) *. (float_of_int((m.hp - 2)))) + 1); max_hp = p.max_hp; 
     mp = p.mp; max_mp = p.max_mp; xp = p.xp; level = p.level; bag = p.bag}
  
  else 
    {name = p.name; classe = p.classe; gender = p.gender; hp = p.hp; max_hp = p.max_hp; 
     mp = p.mp; max_mp = p.max_mp; xp = p.xp; level = p.level; bag = p.bag};;
  
  

let rec fight = fun (p: character) (m: monster) ->
  if (p.classe != Wizard) then
    let a = (hit p m) in
      if (p.hp > 0 && m.hp > 0) then
        fight (monster_hit p a) (a)
      else if (m.hp <= 0 && m.typee = Golem) then
        if (p.xp + m.xp == int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) then
          let () = print_string ("The golem is defeated.\n\nYou gain " ^ string_of_int(m.xp) ^ " experience points.\n\nYou progress to level " ^ string_of_int(p.level + 1) ^ "!\n\n") in
          let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in
          {name = p.name; classe = p.classe; gender = p.gender; hp = hp; max_hp = hp; mp = p.mp; max_mp = p.max_mp; xp = 0; level = p.level + 1; bag = loot_bag p.bag m.loot}
        else if (p.xp + m.xp > int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) then
          let () = print_string ("The golem is defeated.\n\nYou gain " ^ string_of_int(m.xp) ^ " experience points.\n\nYou progress to level " ^ string_of_int(p.level + 1) ^ "!\n\n") in
          let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in
          {name = p.name; classe = p.classe; gender = p.gender; hp = hp; max_hp = hp; mp = p.mp; max_mp = p.max_mp; xp = (p.xp + m.xp) - int_of_float(2. ** float_of_int(p.level + 1) *. 10.); level = p.level + 1; bag = loot_bag p.bag m.loot}
        else
          let () = print_string ("The golem is defeated.\n\nYou gain " ^ string_of_int(m.xp) ^ " experience points.\n\n") in
          {name = p.name; classe = p.classe; gender = p.gender; hp = p.max_hp; max_hp = p.max_hp; mp = p.mp; max_mp = p.max_mp; xp = p.xp + m.xp; level = p.level; bag = loot_bag p.bag m.loot}
      else if (m.hp <= 0 && m.typee = Mosquito) then
        if (p.xp + m.xp == int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) then
          let () = print_string ("The mosquito swarm is defeated.\n\nYou gain " ^ string_of_int(m.xp) ^ " experience points.\n\nYou progress to level " ^ string_of_int(p.level + 1) ^ "!\n\n") in
          let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in
          {name = p.name; classe = p.classe; gender = p.gender; hp = hp; max_hp = hp; mp = p.mp; max_mp = p.max_mp; xp = 0; level = p.level + 1; bag = loot_bag p.bag m.loot}
        else if (p.xp + m.xp > int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) then
          let () = print_string ("The mosquito swarm is defeated.\n\nYou gain " ^ string_of_int(m.xp) ^ " experience points.\n\nYou progress to level " ^ string_of_int(p.level + 1) ^ "!\n\n") in
          let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in
          {name = p.name; classe = p.classe; gender = p.gender; hp = hp; max_hp = hp; mp = p.mp; max_mp = p.max_mp; xp = (p.xp + m.xp) - int_of_float(2. ** float_of_int(p.level + 1) *. 10.); level = p.level + 1; bag = loot_bag p.bag m.loot}
        else
          let () = print_string ("The mosquito swarm is defeated.\n\nYou gain " ^ string_of_int(m.xp) ^ " experience points.\n\n") in
          {name = p.name; classe = p.classe; gender = p.gender; hp = p.max_hp; max_hp = p.max_hp; mp = p.mp; max_mp = p.max_mp; xp = p.xp + m.xp; level = p.level; bag = loot_bag p.bag m.loot}
      else if (m.hp <= 0 && m.typee = Boar) then
        if (p.xp + m.xp == int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) then
          let () = print_string ("The boar is defeated.\n\nYou gain " ^ string_of_int(m.xp) ^ " experience points.\n\nYou progress to level " ^ string_of_int(p.level + 1) ^ "!\n\n") in
          let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in
          {name = p.name; classe = p.classe; gender = p.gender; hp = hp; max_hp = hp; mp = p.mp; max_mp = p.max_mp; xp = 0; level = p.level + 1; bag = loot_bag p.bag m.loot}
        else if (p.xp + m.xp > int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) then
          let () = print_string ("The boar is defeated.\n\nYou gain " ^ string_of_int(m.xp) ^ " experience points.\n\nYou progress to level " ^ string_of_int(p.level + 1) ^ "!\n\n") in
          let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in
          {name = p.name; classe = p.classe; gender = p.gender; hp = hp; max_hp = hp; mp = p.mp; max_mp = p.max_mp; xp = (p.xp + m.xp) - int_of_float(2. ** float_of_int(p.level + 1) *. 10.); level = p.level + 1; bag = loot_bag p.bag m.loot}
        else
          let () = print_string ("The boar is defeated.\n\nYou gain " ^ string_of_int(m.xp) ^ " experience points.\n\n") in
          {name = p.name; classe = p.classe; gender = p.gender; hp = p.max_hp; max_hp = p.max_hp; mp = p.mp; max_mp = p.max_mp; xp = p.xp + m.xp; level = p.level; bag = loot_bag p.bag m.loot}
      else let () = print_string "YOU ARE DEAD!\n\n" in raise YouAreDead
  
  else let a = (wizard_attack p m) in
    
    if (p.mp = a.mp) then let b = (hit a m) in (* Wizard *) 
    
      if (p.hp > 0 && m.hp > 0) then fight (monster_hit {name=p.name;classe=p.classe;gender=p.gender;hp=p.hp;max_hp=p.max_hp;mp=a.mp;max_mp=p.max_mp;xp=p.xp;level=p.level;bag=p.bag} b) (b)   (* p returns xp but not mana and vice versa *)
      
      else if (m.hp <= 0 && m.typee = Golem) then
   
        if (p.xp + m.xp == int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) 
        then let () = print_string ("The golem is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou progress to level "^string_of_int(p.level + 1)^"!\n\n") in let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=0;level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else if (p.xp + m.xp > int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) then let () = print_string ("The golem is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou progress to level "^string_of_int(p.level + 1)^"!\n\n") in let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=(p.xp + m.xp) - int_of_float(2. ** float_of_int(p.level + 1) *. 10.);level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else let () = print_string ("The golem is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\n") in {name=p.name;classe=p.classe;gender=p.gender;hp=p.hp;max_hp=p.max_hp;mp=p.mp;max_mp=p.max_mp;xp=p.xp + m.xp;level=p.level;bag=loot_bag p.bag m.loot}
  
      else if (m.hp <= 0 && m.typee = Mosquito) then 
  
        if (p.xp + m.xp == int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) 
        then let () = print_string ("The swarm of mosquitoes is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou progress to level "^string_of_int(p.level + 1)^"!\n\n") in let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=0;level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else if (p.xp + m.xp > int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) then let () = print_string ("The swarm of mosquitoes is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou progress to level "^string_of_int(p.level + 1)^"!\n\n") in let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=(p.xp + m.xp) - int_of_float(2. ** float_of_int(p.level + 1) *. 10.);level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else let () = print_string ("The swarm of mosquitoes is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\n") in {name=p.name;classe=p.classe;gender=p.gender;hp=p.hp;max_hp=p.max_hp;mp=p.mp;max_mp=p.max_mp;xp=p.xp + m.xp;level=p.level;bag=loot_bag p.bag m.loot}
  
      else if (m.hp <= 0 && m.typee = Boar) then
  
        if (p.xp + m.xp == int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) 
        then let () = print_string ("The boar is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou progress to level "^string_of_int(p.level + 1)^"!\n\n") in let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in  {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=0;level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else if (p.xp + m.xp > int_of_float(2. ** float_of_int(p.level + 1) *. 10.)) then let () = print_string ("The boar is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou progress to level "^string_of_int(p.level + 1)^"!\n\n") in let hp = int_of_float(float_of_int(p.max_hp) *. (5. /. 4.)) in {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=(p.xp + m.xp) - int_of_float(2. ** float_of_int(p.level + 1) *. 10.);level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else let () = print_string ("The boar is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\n") in {name=p.name;classe=p.classe;gender=p.gender;hp=p.hp;max_hp=p.max_hp;mp=p.mp;max_mp=p.max_mp;xp=p.xp + m.xp;level=p.level;bag=loot_bag p.bag m.loot} 

      else let ()=print_string "YOU ARE DEAD!\n\n" in raise YouAreDead
          
    else let c=
           if (p.mp-a.mp=10)  then (cast_spell a m "B")
           else if(p.mp-a.mp=8) then (cast_spell a m "T")
           else (cast_spell a m "P") 
      in
  
      if (p.hp>0 && m.hp>0) then
        
        if (p.mp-a.mp=10)  then fight (monster_hit {name=p.name;classe=p.classe;gender=p.gender;hp=p.hp;max_hp=p.max_hp;mp=a.mp;max_mp=p.max_mp;xp=p.xp;level=p.level;bag=p.bag} c) c
        else if(p.mp-a.mp=8) then fight (monster_hit {name=p.name;classe=p.classe;gender=p.gender;hp=p.hp;max_hp=p.max_hp;mp=a.mp;max_mp=p.max_mp;xp=p.xp;level=p.level;bag=p.bag} c) c
        else fight (monster_hit {name=p.name;classe=p.classe;gender=p.gender;hp=p.hp;max_hp=p.max_hp;mp=a.mp;max_mp=p.max_mp;xp=p.xp;level=p.level;bag=p.bag} c) c
            
      else if (m.hp<=0 && m.typee=Golem) then
   
        if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
        then let () = print_string ("The golem is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou advance to level "^string_of_int(p.level+1)^"!\n\n") in let hp=int_of_float(float_of_int(p.max_hp)*.(5./.4.)) in {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=0;level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("The golem is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou advance to level "^string_of_int(p.level+1)^"!\n\n") in let hp=int_of_float(float_of_int(p.max_hp)*.(5./.4.)) in {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else let () = print_string ("The golem is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\n") in {name=p.name;classe=p.classe;gender=p.gender;hp=p.hp;max_hp=p.max_hp;mp=p.mp;max_mp=p.max_mp;xp=p.xp+m.xp;level=p.level;bag=loot_bag p.bag m.loot}
  
      else if (m.hp<=0 && m.typee=Mosquito) then 
  
        if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
        then let () = print_string ("The swarm of mosquitoes is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou advance to level "^string_of_int(p.level+1)^"!\n\n") in let hp=int_of_float(float_of_int(p.max_hp)*.(5./.4.)) in {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=0;level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("The swarm of mosquitoes is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou advance to level "^string_of_int(p.level+1)^"!\n\n") in let hp=int_of_float(float_of_int(p.max_hp)*.(5./.4.)) in {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else let () = print_string ("The swarm of mosquitoes is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\n") in {name=p.name;classe=p.classe;gender=p.gender;hp=p.hp;max_hp=p.max_hp;mp=p.mp;max_mp=p.max_mp;xp=p.xp+m.xp;level=p.level;bag=loot_bag p.bag m.loot}
  
      else if (m.hp<=0 && m.typee=Boar) then
  
        if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
        then let () = print_string ("The wild boar is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou advance to level "^string_of_int(p.level+1)^"!\n\n") in let hp=int_of_float(float_of_int(p.max_hp)*.(5./.4.)) in  {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=0;level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("The wild boar is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\nYou advance to level "^string_of_int(p.level+1)^"!\n\n") in let hp=int_of_float(float_of_int(p.max_hp)*.(5./.4.)) in {name=p.name;classe=p.classe;gender=p.gender;hp=hp;max_hp=hp;mp=p.mp;max_mp=p.max_mp;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;bag=loot_bag p.bag m.loot}
    
        else let () = print_string ("The wild boar is defeated.\n\nYou gain "^string_of_int(m.xp)^" experience points.\n\n") in {name=p.name;classe=p.classe;gender=p.gender;hp=p.hp;max_hp=p.max_hp;mp=p.mp;max_mp=p.max_mp;xp=p.xp+m.xp;level=p.level;bag=loot_bag p.bag m.loot}  
                                                                                                                                      
      else let ()=print_string "YOU ARE DEAD!\n\n" in raise YouAreDead;;




  
(*------------------------------------------------------------------------GAME FLOW------------------------------------------------------------------------------------------*)


let data = fun (d: string) (p: character) -> let () = print_string (d ^ "\n") in p;;

let end_game = fun p -> print_string "\n\nEnd of the game\n\n";;

let eat_result = fun a -> match a with
  | (true, p) -> p
  | (false, p) -> p;;

let drink_result = fun a -> match a with
  | (true, p) -> p
  | (false, p) -> p;;

let sleep_filter = fun (d: sleep_event) (p: character) -> match d with
  | Character cp -> cp 
  | Eat _ -> p
  | Stats _ -> p
  | Monster _ -> raise YouAreDead;;

let sleep_exception = fun (p: character) ->
  try sleep_filter (sleep p) p
  with YouAreDead -> {name=p.name; classe=p.classe; gender=p.gender; hp=0; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag};;

let fight_exception = fun (p: character) (m: monster) ->
  try fight p m
  with YouAreDead -> {name=p.name; classe=p.classe; gender=p.gender; hp=0; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag};;

let choose_lost_stuff = fun () -> match Random.int(4) with
  | 0 -> Coins
  | 1 -> Chickens
  | 2 -> Sponges
  | 3 -> Nothing
  | _ -> Nothing;;

let rec escape_filter = fun (s: inventory) (o: item) -> match s with
  | [] -> s
  | h::t -> if (o.name = Nothing) then h::t 
      else if (h.name = o.name && h.quantity > 0) then let ()=print_string ("In your haste to escape, you lose "^translate_string o^"\n\n") in {quantity=if (o.quantity >= h.quantity) then 0 else h.quantity-o.quantity; name=h.name}::escape_filter t o 
      else if (h.name = o.name && h.quantity = 0) then escape_filter (h::t) {quantity=(Random.int(2)+1); name=(choose_lost_stuff())} 
      else h::escape_filter t o;;

let escape = fun (p: character) -> let a = escape_filter p.bag {quantity=(Random.int(2)+1); name=(choose_lost_stuff())} in 
  if (a = p.bag) then let () = print_string "You managed to escape without any loss\n\n" in {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag}
  else {name=p.name; classe=p.classe; gender=p.gender; hp=p.hp; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=a};;




let rec unfortunate_encounter = fun (p: character) (m: monster) -> let () = print_string "\n\nWhat do you do?\n\n(A) Attack\n\n(E) escape\n\n(V) View your character's status\n\n<?>\n\n"
  in let i = read_line ()
  in 
  if (p.classe = Wizard) then
    if (i = "A") then fight_exception p m
    else if (i = "E") then 
      if ((Random.int(20)+1) <= 4) then let a = generate() in
        if (a.typee = Golem) then let ()=print_string "Unlucky, while trying to escape, you suddenly encounter a golem.\n" in unfortunate_encounter (escape p) a 
        else if (a.typee = Boar) then let ()=print_string "Unlucky, while trying to escape, you suddenly encounter a wild boar.\n" in unfortunate_encounter (escape p) a
        else let ()=print_string ("Unlucky, while trying to escape, you suddenly encounter a swarm of "^string_of_int(a.hp-2)^" mosquitoes.\n") in unfortunate_encounter (escape p) a
      else escape p
    else if (i = "V") then unfortunate_encounter (data (show_stats p) p) m
    else unfortunate_encounter p m
  else 
  if (i = "A") then fight_exception p m
  else if (i = "F") then 
    if ((Random.int(20)+1) <= 4) then let a = generate() in
      if (a.typee = Golem) then let ()=print_string "Unlucky, while trying to escape, you suddenly encounter a golem.\n" in unfortunate_encounter (escape p) a 
      else if (a.typee = Boar) then let ()=print_string "Unlucky, while trying to escape, you suddenly encounter a wild boar.\n" in unfortunate_encounter (escape p) a
      else let ()=print_string ("Unlucky, while trying to escape, you suddenly encounter a swarm of "^string_of_int(a.hp-2)^" mosquitoes.\n") in unfortunate_encounter (escape p) a
    else escape p
  else if (i = "V") then unfortunate_encounter (data (show_stats p) p) m
  else unfortunate_encounter p m;;

let rec play = fun (p: character) -> if (p.hp != 0 || p.level = 10) then
    let () = print_string "\n\nWhat do you want to do?\n\n(C) Continue your journey\n\n(S) Sleep\n\n(E) Eat\n\n(D) Drink\n\n(V) View your character's status\n\n(Q) Quit the adventure\n\n<?>\n"  
    in let i = read_line () 
    in 
    if (i = "S") then play (sleep_exception p)
    else if (i = "E") then  play (eat_result (eat p))
    else if (i = "D") then  play (drink_result (drink p))
    else if (i = "V") then  play (data (show_stats p) p)
    else if (i = "C") then let a = generate() in 
      if (a.typee = Golem) then let ()=print_string "\nYou decide to continue your journey when suddenly you hear noise at the edge of the forest and you see a golem." in play (unfortunate_encounter p a)
      else if (a.typee = Boar) then let ()=print_string "\nYou decide to continue your journey when suddenly you hear noise at the edge of the forest and you see a wild boar." in play (unfortunate_encounter p a)
      else let ()=print_string ("\nYou decide to continue your journey when suddenly you hear noise at the edge of the forest and you see a swarm of "^string_of_int(a.hp-2)^" mosquitoes.") in play (unfortunate_encounter p a)
    else if (i = "Q") then end_game p
    else play p
  else end_game {name=p.name; classe=p.classe; gender=p.gender; hp=0; max_hp=p.max_hp; mp=p.mp; max_mp=p.max_mp; xp=p.xp; level=p.level; bag=p.bag};;


get_name;;
get_classe;;
get_gender;;
play (manage_mana player);;
