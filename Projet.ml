Random.self_init ();;
   

let rec iterate : (int * ('a->'a) * 'a) -> 'a =
  fun (count, f, initial_value) ->
    if count <= 0
    then initial_value
    else iterate (count-1, f, f initial_value)
;; 

let repeat_string=fun (x,y)->iterate(y,(fun s->s^x),x);;

type classe=Archer|Guerrier|Magicien;;
type genre=Masculin|Feminin;;
type obj=Rien|Eponges|Poulets|Pieces|Potion_de_soin|Potion_de_magie;;
type objet={quantite:int;nom:obj};;
type inventaire=objet list;;
type personnage={nom:string;classe:classe;genre:genre;pv:int;pv_max:int;pm:int;pm_max:int;xp:int;level:int;sac:inventaire};;
type race=Golem|Moustique|Sanglier;;
type monstre={typee:race;pv:int;xp:int;loot:objet};;
type dodo=Personnage of personnage|Monstre of monstre|Manger of (bool*personnage)|Stats of string;; 
                                                     

let classgender=fun r->match r with
  |{classe=Archer}->if (r.genre=Masculin) then "Archer" else "Archère"
  |{classe=Guerrier}->if (r.genre=Masculin) then "Guerrier" else "Guerrière"
  |{classe=Magicien}->if (r.genre=Masculin) then "Magicien" else "Magiciène";;


let entete=fun (p:personnage)->if (p.level<10) then"+"^repeat_string("-",String.length(p.nom)+String.length(classgender p)+String.length("niveau 10")+4)^"+"
  else "+"^repeat_string("-",String.length(p.nom)+String.length(classgender p)+String.length("niveau 10")+5)^"+" ;;
  
let name=fun ()->let ()=print_string "\n\nVeuillez saisir votre nom : "
  in let i=read_line()
  in i;;

let gender=fun ()->let ()=print_string "\n\nChoisissez le genre de votre personnage : \n\n1. Homme\n2. Femme\n"
  in let i=read_int()
  in if(i==1) then Masculin
  else Feminin;;

let classe=fun ()->let ()=print_string "\n\nVeuillez choisir votre classe parmi les suivantes : \n\n1. Guerrier\n2. Archer\n3. Magicien\n"
  in let i=read_int()
  in 
  if(i==1) then Guerrier
  else if (i==2) then Archer 
  else Magicien;;

let b:inventaire=[{quantite=0;nom=Poulets};{quantite=0;nom=Pieces};{quantite=0;nom=Eponges};
                  {quantite=0;nom=Potion_de_soin};{quantite=0;nom=Potion_de_magie}];;
let f={nom=name();classe=classe();genre=gender();pv=20;pv_max=20;pm=0;pm_max=40;xp=0;level=1;sac=b};;


let mana= fun (p:personnage)-> 
  if (p.classe=Magicien) then {nom=p.nom;classe=p.classe;genre=p.genre;pv=20;pv_max=20;pm=40;pm_max=40;xp=0;level=1;sac=b}
  else {nom=p.nom;classe=p.classe;genre=p.genre;pv=20;pv_max=20;pm=0;pm_max=0;xp=0;level=1;sac=b};;


let transtring=fun (o:objet)->match o with
  |{quantite=int;nom=Pieces}->if (o.quantite>1)then string_of_int(o.quantite)^" pièces"  else string_of_int(o.quantite)^" pièce"
  |{quantite=int;nom=Poulets}->if (o.quantite>1)then string_of_int(o.quantite)^" poulets"  else string_of_int(o.quantite)^" poulet"
  |{quantite=int;nom=Eponges}->if (o.quantite>1)then string_of_int(o.quantite)^" éponges"  else string_of_int(o.quantite)^" éponge"
  |{quantite=int;nom=Potion_de_soin}->if (o.quantite>1)then string_of_int(o.quantite)^" Potions de soin"  else string_of_int(o.quantite)^" Potion de soin"
  |{quantite=int;nom=Potion_de_magie}->if (o.quantite>1)then string_of_int(o.quantite)^" Potions de magie"  else string_of_int(o.quantite)^" Potion de magie"
  |{quantite=int;nom=_}->"";;

let rec stuff=fun (s:inventaire) (p:personnage)->match s with
  |[]->"" 
  |h::t->if (h.nom=Poulets) then "|  "^transtring h^repeat_string(" ",String.length(entete p)-String.length(transtring h)-5)^"|"^"\n"^stuff t p
      else if(h.nom=Pieces) then "|  "^transtring h^repeat_string(" ",String.length(entete p)-String.length(transtring h)-4)^"|"^"\n"^stuff t p
      else if (h.nom=Potion_de_soin) then "|  "^transtring h^repeat_string(" ",String.length(entete p)-String.length(transtring h)-5)^"|"^"\n"^stuff t p
      else if (h.nom=Potion_de_magie) then "|  "^transtring h^repeat_string(" ",String.length(entete p)-String.length(transtring h)-5)^"|"^"\n"^stuff t p
      else "|  "^transtring h^repeat_string(" ",String.length(entete p)-String.length(transtring h)-4)^"|"^"\n"^stuff t p;;


let stats=fun (p:personnage)->
  if (p.genre=Masculin && p.classe=Magicien) then 
    "\n"^entete p^"\n| "^p.nom^" | "^classgender p^" niveau "^string_of_int(p.level)^" |\n"^
    entete p^"\n| Points de vie |"^repeat_string(" ",String.length(entete p)-String.length("| Points de vie |")-8)^string_of_int(p.pv)^"/"^string_of_int(p.pv_max)^" |\n"^
    entete p^"\n| Points de magie |"^repeat_string(" ",String.length(entete p)-String.length("| Points de magie |")-8)^string_of_int(p.pm)^"/"^string_of_int(p.pm_max)^" |\n"^
    entete p^"\n| Experience    |"^repeat_string(" ",String.length(entete p)-String.length("| Points de vie |")-4)^string_of_int(p.xp)^" |\n"^
    entete p^"\n| Sac"^repeat_string(" ",String.length(entete p)-String.length("| Sac")-2)^"|\n"^
    stuff p.sac p^entete p
      
  else if (p.genre=Feminin && p.classe=Magicien) then 
    "\n"^entete p^"\n| "^p.nom^" | "^classgender p^" niveau "^string_of_int(p.level)^"  |\n"^
    entete p^"\n| Points de vie |"^repeat_string(" ",String.length(entete p)-String.length("| Points de vie |")-8)^string_of_int(p.pv)^"/"^string_of_int(p.pv_max)^" |\n"^
    entete p^"\n| Points de magie |"^repeat_string(" ",String.length(entete p)-String.length("| Points de magie |")-8)^string_of_int(p.pm)^"/"^string_of_int(p.pm_max)^" |\n"^
    entete p^"\n| Experience    |"^repeat_string(" ",String.length(entete p)-String.length("| Points de vie |")-4)^string_of_int(p.xp)^" |\n"^
    entete p^"\n| Sac"^repeat_string(" ",String.length(entete p)-String.length("| Sac")-2)^"|\n"^
    stuff p.sac p^entete p^"\n"
    
  else if (p.genre=Masculin && p.classe!=Magicien) then
    "\n"^entete p^"\n| "^p.nom^" | "^classgender p^" niveau "^string_of_int(p.level)^" |\n"^
    entete p^"\n| Points de vie |"^repeat_string(" ",String.length(entete p)-String.length("| Points de vie |")-8)^string_of_int(p.pv)^"/"^string_of_int(p.pv_max)^" |\n"^
    entete p^"\n| Experience    |"^repeat_string(" ",String.length(entete p)-String.length("| Points de vie |")-4)^string_of_int(p.xp)^" |\n"^
    entete p^"\n| Sac"^repeat_string(" ",String.length(entete p)-String.length("| Sac")-2)^"|\n"^
    stuff p.sac p^entete p
      
  else "\n"^entete p^"\n| "^p.nom^" | "^classgender p^" niveau "^string_of_int(p.level)^"  |\n"^
       entete p^"\n| Points de vie |"^repeat_string(" ",String.length(entete p)-String.length("| Points de vie |")-8)^string_of_int(p.pv)^"/"^string_of_int(p.pv_max)^" |\n"^entete p^"\n| Experience    |"^repeat_string(" ",String.length(entete p)-String.length("| Points de vie |")-4)^string_of_int(p.xp)^" |\n"^
       entete p^"\n| Sac"^repeat_string(" ",String.length(entete p)-String.length("| Sac")-2)^"|\n"^
       stuff p.sac p^entete p^"\n";;
       

let genere_loot=fun ()->match Random.int(6) with
  |0->{quantite=Random.int(10)+1;nom=Pieces}
  |1->{quantite=Random.int(5)+1;nom=Poulets}
  |2->{quantite=Random.int(10)+1;nom=Eponges}
  |3->{quantite=0;nom=Rien}
  |4->{quantite=Random.int(2)+1;nom=Potion_de_soin}
  |5->{quantite=Random.int(2)+1;nom=Potion_de_magie}
  |_->{quantite=0;nom=Rien};;

let genere=fun ()->match Random.int(3) with 
  |0->{typee=Golem;pv=25+Random.int(6)+1;xp=8;loot=genere_loot()}
  |1->let nb_moustiques = (Random.int(8)+3) in
      {typee=Moustique;pv=2+nb_moustiques;xp=2;loot={quantite=0;nom=Rien}}
  |2->{typee=Sanglier;pv=10+Random.int(4)+1;xp=4;loot=genere_loot()}
  |_->{typee=Sanglier;pv=10;xp=4;loot=genere_loot()};; 

                         

  
(*------------------------------------------------------------------------------GESTION DES SORTS-------------------------------------------------------------------------------------*)

let rec choixSort=fun (p:personnage)->let () = print_string "\n\nQuel sort voulez vous lancer ?\n\n(B) Boule de feu\n\n(P) Pique de glace\n\n(T) Tempete de feu\n\n"
  in let i=read_line () 
  in
  if (i="B") then 
    if (p.pm<10) then let ()=print_string "Pas assez de mana" in choixSort p 
    else i
  else if(i="P") then 
    if (p.pm<5) then let ()=print_string "Pas assez de mana" in choixSort p 
    else i
  else if(i="T") then 
    if (p.pm<8) then let ()=print_string "Pas assez de mana" in choixSort p 
    else i
  else choixSort p;;

let rec pertePm=fun (p:personnage) x-> 
  if(x="B") then {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=20;pm=p.pm-10;pm_max=40;xp=0;level=1;sac=b}
  else if(x="P") then {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=20;pm=p.pm-5;pm_max=40;xp=0;level=1;sac=b}
  else if(x="T") then {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=20;pm=p.pm-8;pm_max=40;xp=0;level=1;sac=b}
  else pertePm p x;;

let rec coup_magicien=fun (p:personnage) (m:monstre)->
  if (m.pv>0 && p.pv>0) then let ()=print_string "\n\nQue faites-vous ?\n\n(C) Coup a la main\n\n(S) Lancer un sort\n\n" in 
    let i=read_line() 
    in
    if(i="C") then p 
    else if (i="S") then 
      if (p.pm<5) then let ()=print_string "\n\nVous n'avez pas assez de mana pour lancer un sort\n\n" in coup_magicien p m 
      else pertePm p (choixSort p)
    else coup_magicien p m
  else p;;

let rec sort=fun (p:personnage) (m:monstre) x-> 
  if (p.pv>0 && m.pv>0) then 
    if(x="B") then
      if((Random.int(20)+1)<=if (2+p.level*1)<20 then 10+p.level*1 else 20) then let () = print_string "\n\nVous lancez une boule de feu et faite 12 de dégats.\n\n" in {typee=m.typee;pv=m.pv-12;xp=m.xp;loot=m.loot} 
      else let () = print_string "\n\nVous ripostez, mais vous manquez la cible.\n\n" in {typee=m.typee;pv=m.pv;xp=m.xp;loot=m.loot} 
  
    else if(x="P") then
      if ((Random.int(20)+1)<=if (6+p.level*1)<20 then 10+p.level*1 else 20) then let () = print_string "\n\nVous lancez un pique et faite 7 de dégats .\n\n" in {typee=m.typee;pv=m.pv-7;xp=m.xp;loot=m.loot} 
      else let () = print_string "\n\nVous ripostez, mais vous manquez la cible.\n\n" in {typee=m.typee;pv=m.pv;xp=m.xp;loot=m.loot}
  
    else if(x="T") then
      if((Random.int(20)+1)<=if (4+p.level*1)<20 then 10+p.level*1 else 20) then let () = print_string "\n\nVous lancez une tempete et faite 9 de dégats .\n\n" in {typee=m.typee;pv=m.pv-9;xp=m.xp;loot=m.loot} 
      else let () = print_string "\n\nVous ripostez, mais vous manquez la cible.\n\n" in {typee=m.typee;pv=m.pv;xp=m.xp;loot=m.loot}
    else sort p m x
  else {typee=m.typee;pv=m.pv;xp=m.xp;loot=m.loot};; 
                                                                                                                                                                                                                 
  
  
  
  (*------------------------------------------------------------------------------RÉGÉNÉRATION--------------------------------------------------------------------------------------*)

let rec filtre_quantite_manger=fun s:inventaire->match s with
  |[]->s
  |h::t->if (h.nom=Poulets && h.quantite>0) then h::t else filtre_quantite_manger t;;
  
let rec filtre_manger=fun s:inventaire->match s with
  |[]->s
  |h::t->if (h.nom=Poulets && h.quantite>0) then {quantite=h.quantite-1;nom=h.nom}::filtre_manger t else h::filtre_manger t;;

let rec filtre_quantiteVie=fun s:inventaire->match s with
  |[]->s
  |h::t->if (h.nom=Potion_de_soin && h.quantite>0) then h::t else filtre_quantiteVie t;;
  
let rec filtre_quantiteMagie=fun s:inventaire->match s with
  |[]->s
  |h::t->if (h.nom=Potion_de_magie && h.quantite>0) then h::t else filtre_quantiteMagie t;;

let rec filtre_boireVie=fun s:inventaire->match s with
  |[]->s
  |h::t->if (h.nom=Potion_de_soin && h.quantite>0) then {quantite=h.quantite-1;nom=h.nom}::filtre_boireVie t else h::filtre_boireVie t;;
  
let rec filtre_boireMagie=fun s:inventaire->match s with
  |[]->s
  |h::t->if (h.nom=Potion_de_magie && h.quantite>0) then {quantite=h.quantite-1;nom=h.nom}::filtre_boireMagie t else h::filtre_boireMagie t;;
  
  
let rec boire=fun (p:personnage)->let ()=print_string "\n\nChoisissez si vous voulez boire une potion de vie ou de magie : \n\n1.(V) Potion de vie,elle redonne jusqu'a 10 pt vie\n2.(M) Magie,elle redonne jusqu'a 20 pt vie\n"
  in let i=read_line() in
  if(i="V") then
    if (p.pv==20) then let () = print_string "\n\nVous ne pouvez pas boire.\n\n" in (false,p) 
  
    else if ((p.pv<=19) && (p.pv>15) && (filtre_quantiteVie p.sac!=[])) then let () = print_string ("\n\nVous avez bu 1 potion de vie et gagnez "^string_of_int(20-p.pv)^" points de vie.\n\n") in (true,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+(20-p.pv);pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=filtre_boireVie p.sac})
  
    else if ((p.pv<=19) && (p.pv>15) && (filtre_quantiteVie p.sac==[])) then let () = print_string "\n\nVous ne pouvez pas boire.\n\n" in (false,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac})
  
    else if ((p.pv<=15) && (filtre_quantiteVie p.sac!=[]))  then let () = print_string "\n\nVous avez bu 1 potion de vie et gagnez 5 points de vie.\n\n" in (true,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+5;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=filtre_boireVie p.sac})
  
    else let () = print_string "\n\nVous ne pouvez pas boire.\n\n" in (false,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv_max;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac})
  
  else if (i="M") then 
    if (p.classe=Magicien) then
    
      if (p.pm==40) then let () = print_string "\n\nVous ne pouvez pas boire.\n\n" in (false,p) 
  
      else if ((p.pm<=39) && (p.pm>30) && (filtre_quantiteMagie p.sac!=[])) then let () = print_string ("\n\nVous avez bu 1 potion de magie et gagnez "^string_of_int(40-p.pm)^" points de mana.\n\n") in (true,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm+(40-p.pm);pm_max=p.pm_max;xp=p.xp;level=p.level;sac=filtre_boireMagie p.sac})
  
      else if ((p.pm<=39) && (p.pm>30) && (filtre_quantiteMagie p.sac==[])) then let () = print_string "\n\nVous ne pouvez pas boire.\n\n" in (false,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac})
  
      else if ((p.pm<=30) && (filtre_quantiteMagie p.sac!=[]))  then let () = print_string "\n\nVous avez bu 1 potion de magie et gagnez 10 points de mana.\n\n" in (true,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm+10;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=filtre_boireMagie p.sac}) 
                                                                                                                                                                
      else let () = print_string "\n\nVous ne pouvez pas boire.\n\n" in (false,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac})
    
    else let ()=print_string "\n\nVous n'avez pas de mana" in boire p
      
  else boire p;;


let manger=fun (p:personnage)->
  if (p.pv==20) then let () = print_string "Vous ne pouvez pas manger.\n\n" in (false,p) 
  
  else if (p.pv==19 && (filtre_quantite_manger p.sac!=[])) then let () = print_string "Vous avez mangé 1 poulet et gagnez 2 points de vie.\n\n" in (true,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+1;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=filtre_manger p.sac})
  
  else if (p.pv==19 && (filtre_quantite_manger p.sac==[])) then let () = print_string "Vous ne pouvez pas manger.\n\n" in (false,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac})
  
  else if (p.pv<19 && (filtre_quantite_manger p.sac!=[]))  then let () = print_string "Vous avez mangé 1 poulet et gagnez 2 points de vie.\n\n" in (true,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+2;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=filtre_manger p.sac})
  
  else let () = print_string "Vous ne pouvez pas manger.\n\n" in (false,{nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac});;

let reposer = fun (p:personnage)->
  
  if (p.pv==20) then 
    if (p.pm==40) then Personnage p
    else if ((p.pm<=39) && (p.pm>32)) then Personnage {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm+(40-p.pm);pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
    else Personnage {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm+8;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
  
  else if (p.pv==19) then 
    if (p.pm==40) then Personnage p
    else if ((p.pm<=39) && (p.pm>32)) then Personnage {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+1;pv_max=p.pv_max;pm=p.pm+(40-p.pm);pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
    else Personnage {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+1;pv_max=p.pv_max;pm=p.pm+8;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
  
  else if (p.pv==18) then
    if (p.pm==40) then Personnage p
    else if ((p.pm<=39) && (p.pm>32)) then Personnage {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+2;pv_max=p.pv_max;pm=p.pm+(40-p.pm);pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
    else Personnage {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+2;pv_max=p.pv_max;pm=p.pm+8;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
  
  else if (p.pv==17) then 
    if (p.pm==40) then Personnage p
    else if ((p.pm<=39) && (p.pm>32)) then Personnage {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+3;pv_max=p.pv_max;pm=p.pm+(40-p.pm);pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
    else Personnage {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+3;pv_max=p.pv_max;pm=p.pm+8;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
  
  else 
  if (p.pm==40) then Personnage p 
  else if ((p.pm<=39) && (p.pm>32)) then Personnage {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+4;pv_max=p.pv_max;pm=p.pm+(40-p.pm);pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
  else Personnage {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv+4;pv_max=p.pv_max;pm=p.pm+8;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac};;

exception YouAreDead
  
let dormir = fun (p:personnage)->
  if ((Random.int(20)+1)<=19) 
  then let () = print_string "\nVous installez votre campement et tombez rapidement endormie.\nVous vous réveillez le lendemain matin et gagnez 4 points de vie et 8 points de mana.\n\n" in reposer p
  
  else if (Monstre (genere())==Monstre {typee=Golem;pv=(genere()).pv;xp=(genere()).xp;loot=(genere()).loot}) 
  then let () = print_string "\nVous installez votre campement et tombez rapidement endormie.\n\nPendant votre sommeil, un golem surgit et vous fracasse le crâne.\n\nVOUS ÊTES MORT.E !\n\n" in Monstre (genere())
  
  else if (Monstre (genere())==Monstre {typee=Moustique;pv=(genere()).pv;xp=(genere()).xp;loot=(genere()).loot})
  then let () = print_string "\nVous installez votre campement et tombez rapidement endormie.\n\nPendant votre sommeil, une nuée de moustiques vous a vidé de votre sang.\n\nVOUS ÊTES MORT.E !\n\n" in Monstre (genere())
  
  else let () = print_string "\nVous installez votre campement et tombez rapidement endormie.\n\nPendant votre sommeil, un sanglier s'est assis sur vous.\n\nVOUS ÊTES MORT.E !\n\n" in Monstre (genere());;


(*----------------------------------------------------------------------------------COMBAT-----------------------------------------------------------------------------------------------*)


let rec loot_bag=fun (l:inventaire) (o:objet)->match l with
  |[]->l
  |h::t->if (h.nom=o.nom) then {quantite=h.quantite+o.quantite;nom=h.nom}::t else h::loot_bag t o;; 

let frapper=fun (p:personnage) (m:monstre)->
  if (p.classe=Guerrier && p.pv>0 && m.pv>0) then if ((Random.int(20)+1)<=if (6+p.level*1)<20 then 6+p.level*1 else 20) then let () = print_string "Vous touchez et infligez 10 points de dégats.\n\n" in {typee=m.typee;pv=m.pv-10;xp=m.xp;loot=m.loot} else let () = print_string "Vous ripostez, mais vous manquez la cible.\n\n" in {typee=m.typee;pv=m.pv;xp=m.xp;loot=m.loot}

  else if (p.classe=Archer && p.pv>0 && m.pv>0) then if ((Random.int(20)+1)<=if (14+p.level*1)<20 then 14+p.level*1 else 20) then let () = print_string "Vous touchez et infligez 4 points de dégats.\n\n" in {typee=m.typee;pv=m.pv-4;xp=m.xp;loot=m.loot} else let () = print_string "Vous ripostez, mais vous manquez la cible.\n\n" in {typee=m.typee;pv=m.pv;xp=m.xp;loot=m.loot}
  
  else if (p.classe=Magicien && p.pv>0 && m.pv>0) then if((Random.int(20)+1)<=if (10+p.level*1)<20 then 10+p.level*1 else 20) then let () = print_string "Vous touchez et infligez 5 points de dégats.\n\n" in {typee=m.typee;pv=m.pv-5;xp=m.xp;loot=m.loot} else let () = print_string "Vous ripostez, mais vous manquez la cible.\n\n" in {typee=m.typee;pv=m.pv;xp=m.xp;loot=m.loot}
  
  else {typee=m.typee;pv=m.pv;xp=m.xp;loot=m.loot};;

let monstre_frapper=fun (p:personnage) (m:monstre)->
  if (m.typee=Golem && m.pv>0 && p.pv>0) then let () = print_string "Le golem lève son poing et assène un coup puissant, vous perdez 4 points de vie.\n\n" in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv-4;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
  
  else if (m.typee=Sanglier && m.pv>0 && p.pv>0) then let () = print_string "Le sanglier charge et vous renverse, vous perdez 2 points de vie.\n\n" in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv-2;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
  
  else if (m.typee=Moustique && m.pv>0 && p.pv>0) then let () = print_string ("La nuée de moustiques fait mouche, vous perdez "^string_of_int((int_of_float((1./.2.)*.(float_of_int((m.pv-2))))+1))^" points de vie.\n\n") in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv-(int_of_float((1./.2.)*.(float_of_int((m.pv-2))))+1);pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
  
  else {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac};;  

let rec combattre=fun (p:personnage) (m:monstre)-> 
  if(p.classe!=Magicien) then let a=(frapper p m) in 
    if (p.pv>0 && m.pv>0) then combattre (monstre_frapper p a) (a)  

    else if (m.pv<=0 && m.typee=Golem) then
   
      if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
      then let () = print_string ("Le golem est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=0;level=p.level+1;sac=loot_bag p.sac m.loot}
    
      else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("Le golem est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;sac=loot_bag p.sac m.loot}
    
      else let () = print_string ("Le golem est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\n") in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp+m.xp;level=p.level;sac=loot_bag p.sac m.loot}
  
    else if (m.pv<=0 && m.typee=Moustique) then 
  
      if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
      then let () = print_string ("La nuée de moustiques est terrassée.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=0;level=p.level+1;sac=loot_bag p.sac m.loot}
    
      else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("La nuée de moustiques est terrassée.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;sac=loot_bag p.sac m.loot}
    
      else let () = print_string ("La nuée de moustiques est terrassée.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\n") in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp+m.xp;level=p.level;sac=loot_bag p.sac m.loot}
  
    else if (m.pv<=0 && m.typee=Sanglier) then
  
      if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
      then let () = print_string ("Le sanglier est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in  {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=0;level=p.level+1;sac=loot_bag p.sac m.loot}
    
      else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("Le sanglier est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;sac=loot_bag p.sac m.loot}
    
      else let () = print_string ("Le sanglier est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\n") in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp+m.xp;level=p.level;sac=loot_bag p.sac m.loot}
  
    else let ()=print_string "VOUS ÊTES MORT.E !\n\n" in raise YouAreDead
  
  else let a=(coup_magicien p m) in 
    
    if (p.pm==a.pm) then let b=(frapper a m) in (* MAGICIEN *) 
    
      if (p.pv>0 && m.pv>0) then combattre (monstre_frapper {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=a.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac} b) (b)   (* p ça renvoie xp mais pas mana et a l'inverse *)
      
      else if (m.pv<=0 && m.typee=Golem) then
   
        if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
        then let () = print_string ("Le golem est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=0;level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("Le golem est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else let () = print_string ("Le golem est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\n") in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp+m.xp;level=p.level;sac=loot_bag p.sac m.loot}
  
      else if (m.pv<=0 && m.typee=Moustique) then 
  
        if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
        then let () = print_string ("La nuée de moustiques est terrassée.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=0;level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("La nuée de moustiques est terrassée.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else let () = print_string ("La nuée de moustiques est terrassée.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\n") in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp+m.xp;level=p.level;sac=loot_bag p.sac m.loot}
  
      else if (m.pv<=0 && m.typee=Sanglier) then
  
        if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
        then let () = print_string ("Le sanglier est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in  {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=0;level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("Le sanglier est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else let () = print_string ("Le sanglier est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\n") in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp+m.xp;level=p.level;sac=loot_bag p.sac m.loot} 

      else let ()=print_string "VOUS ÊTES MORT.E !\n\n" in raise YouAreDead
          
    else let c=
           if (p.pm-a.pm=10)  then (sort a m "B")
           else if(p.pm-a.pm=8) then (sort a m "T")
           else (sort a m "P") 
      in
  
      if (p.pv>0 && m.pv>0) then
        
        if (p.pm-a.pm=10)  then combattre (monstre_frapper {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=a.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac} c) c
        else if(p.pm-a.pm=8) then combattre (monstre_frapper {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=a.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac} c) c
        else combattre (monstre_frapper {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=a.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac} c) c
            
      else if (m.pv<=0 && m.typee=Golem) then
   
        if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
        then let () = print_string ("Le golem est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=0;level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("Le golem est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else let () = print_string ("Le golem est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\n") in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp+m.xp;level=p.level;sac=loot_bag p.sac m.loot}
  
      else if (m.pv<=0 && m.typee=Moustique) then 
  
        if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
        then let () = print_string ("La nuée de moustiques est terrassée.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=0;level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("La nuée de moustiques est terrassée.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else let () = print_string ("La nuée de moustiques est terrassée.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\n") in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp+m.xp;level=p.level;sac=loot_bag p.sac m.loot}
  
      else if (m.pv<=0 && m.typee=Sanglier) then
  
        if (p.xp+m.xp==int_of_float(2.**float_of_int(p.level+1)*.10.)) 
        then let () = print_string ("Le sanglier est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in  {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=0;level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else if (p.xp+m.xp>int_of_float(2.**float_of_int(p.level+1)*.10.)) then let () = print_string ("Le sanglier est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\nVous progressez au niveau "^string_of_int(p.level+1)^"!\n\n") in let pv=int_of_float(float_of_int(p.pv_max)*.(5./.4.)) in {nom=p.nom;classe=p.classe;genre=p.genre;pv=pv;pv_max=pv;pm=p.pm;pm_max=p.pm_max;xp=(p.xp+m.xp)-int_of_float(2.**float_of_int(p.level+1)*.10.);level=p.level+1;sac=loot_bag p.sac m.loot}
    
        else let () = print_string ("Le sanglier est terrassé.\n\nVous gagnez "^string_of_int(m.xp)^" points d'expérience.\n\n") in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp+m.xp;level=p.level;sac=loot_bag p.sac m.loot}  
                                                                                                                                      
      else let ()=print_string "VOUS ÊTES MORT.E !\n\n" in raise YouAreDead;;




  
  (*------------------------------------------------------------------------DÉROULEMENT DU JEU------------------------------------------------------------------------------------------*)


let donnee=fun (d:string) (p:personnage)->let () = print_string (d^"\n") in p;;

let end_game=fun p->print_string "\n\nFin de la partie\n\n";;

let fil_manger=fun a->match a with
  |(true,p)->p
  |(false,p)->p;;
  
let fil_boire=fun a->match a with
  |(true,p)->p
  |(false,p)->p;;

let filtre_dormir=fun (d:dodo) (p:personnage)->match d with
  |Personnage p->p 
  |Manger _->p
  |Stats _->p
  |Monstre _->raise YouAreDead;; 

let excep_dormir =fun (p:personnage)->
  try filtre_dormir (dormir p) p
  with YouAreDead->{nom=p.nom;classe=p.classe;genre=p.genre;pv=0;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac};;

let excep_combat =fun (p:personnage) (m:monstre)->
  try combattre p m
  with YouAreDead->{nom=p.nom;classe=p.classe;genre=p.genre;pv=0;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac};;
  
let choix_stuff_perdu=fun ()->match Random.int(4) with
  |0->Pieces
  |1->Poulets
  |2->Eponges
  |3->Rien
  |_->Rien;; 

let rec filtre_fuite=fun (s:inventaire) (o:objet)->match s with
  |[]->s
  |h::t->if (o.nom=Rien) then h::t 
      else if (h.nom=o.nom && h.quantite>0) then let ()=print_string ("Dans votre précipitation à fuir vous perdez "^transtring o^"\n\n") in {quantite=if (o.quantite>=h.quantite) then 0 else h.quantite-o.quantite;nom=h.nom}::filtre_fuite t o 
      else if (h.nom=o.nom && h.quantite==0) then filtre_fuite (h::t) {quantite=(Random.int(2)+1);nom=(choix_stuff_perdu())} 
      else h::filtre_fuite t o;;

let fuir=fun (p:personnage)->let a=filtre_fuite p.sac {quantite=(Random.int(2)+1);nom=(choix_stuff_perdu())} in 
  if (a=p.sac) then let () = print_string "Vous avez réussi à fuir sans aucune perte\n\n" in {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac}
  else {nom=p.nom;classe=p.classe;genre=p.genre;pv=p.pv;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=a};;



let rec malheureuse_rencontre=fun (p:personnage) (m:monstre)->let () = print_string "\n\nQue faites-vous ?\n\n(A) Attaquer\n\n(F) Fuir\n\n(V) Visualiser l'état de votre personnage\n\n<?>\n\n"
  in let i=read_line ()
  in 
  if (p.classe=Magicien) then
    if (i="A") then excep_combat p m
    else if (i="F") then 
      if ((Random.int(20)+1)<=4) then let a=genere() in
        if (a.typee=Golem) then let ()=print_string "Pas de chance, en essayant de fuir vous croisez soudain un golem.\n" in malheureuse_rencontre (fuir p) a 
        else if (a.typee=Sanglier) then let ()=print_string "Pas de chance, en essayant de fuir vous croisez soudain un sanglier.\n" in malheureuse_rencontre (fuir p) a
        else let ()=print_string ("Pas de chance, en essayant de fuir vous croisez soudain une nuée de "^string_of_int(a.pv-2)^" moustiques.\n") in malheureuse_rencontre (fuir p) a
      else fuir p
    else if(i="V") then malheureuse_rencontre (donnee (stats p) p) m
    else malheureuse_rencontre p m
  else 
  if (i="A") then excep_combat p m
  else if (i="F") then 
    if ((Random.int(20)+1)<=4) then let a=genere() in
      if (a.typee=Golem) then let ()=print_string "Pas de chance, en essayant de fuir vous croisez soudain un golem.\n" in malheureuse_rencontre (fuir p) a 
      else if (a.typee=Sanglier) then let ()=print_string "Pas de chance, en essayant de fuir vous croisez soudain un sanglier.\n" in malheureuse_rencontre (fuir p) a
      else let ()=print_string ("Pas de chance, en essayant de fuir vous croisez soudain une nuée de "^string_of_int(a.pv-2)^" moustiques.\n") in malheureuse_rencontre (fuir p) a
    else fuir p
  else if(i="V") then malheureuse_rencontre (donnee (stats p) p) m
  else malheureuse_rencontre p m;;



let rec jouer=fun (p:personnage)->if (p.pv!=0 || p.level==10) then
    let () = print_string "\n\nQue voulez-vous faire ?\n\n(C) Continuer votre chemin\n\n(D) Dormir\n\n(M) Manger\n\n(B) Boire\n\n(V) Visualiser l'état de votre personnage\n\n(Q) Quitter l'aventure\n\n<?>\n"  
    in let i = read_line () 
    in 
    if (i="D") then jouer (excep_dormir p)
    else if (i="M") then  jouer (fil_manger (manger p))
    else if (i="B") then  jouer (fil_boire (boire p))
    else if(i="V") then  jouer (donnee (stats p) p)
    else if (i="C") then let a=genere() in 
      if (a.typee=Golem) then let ()=print_string "\nVous décidez de continuez votre chemin quand soudain vous entendez du bruit à l'orée de la forêt et vous apercevez un golem." in jouer (malheureuse_rencontre p a)
      else if (a.typee=Sanglier) then let ()=print_string "\nVous décidez de continuez votre chemin quand soudain vous entendez du bruit à l'orée de la forêt et vous apercevez un sanglier." in jouer (malheureuse_rencontre p a)
      else let ()=print_string ("\nVous décidez de continuez votre chemin quand soudain vous entendez du bruit à l'orée de la forêt et vous apercevez une nuée de "^string_of_int(a.pv-2)^" moustiques.") in jouer (malheureuse_rencontre p a)
    else if(i="Q") then end_game p
    else jouer p
  else end_game {nom=p.nom;classe=p.classe;genre=p.genre;pv=0;pv_max=p.pv_max;pm=p.pm;pm_max=p.pm_max;xp=p.xp;level=p.level;sac=p.sac};;

name;;
classe;;
gender;;
jouer (mana f);;
