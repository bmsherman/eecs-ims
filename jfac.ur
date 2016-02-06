(* Organizing dinners for the MIT EECS junior faculty *)

open Bootstrap3
structure Theme = Ui.Make(Default)

table user : { HumanName : string, Email : string, IsAdmin : bool }
  PRIMARY KEY Email

val userShow : show {HumanName : string} = mkShow (fn r => r.HumanName)
val userRead : read {HumanName : string} = mkRead' (fn s => Some {HumanName = s}) "user"

(* Bootstrap the database with an initial admin user. *)
task initialize = fn () =>
  anyUsers <- oneRowE1 (SELECT COUNT( * ) > 0
                        FROM user);
  if anyUsers then
      return ()
  else
      dml (INSERT INTO user(HumanName, Email, IsAdmin)
           VALUES ('Ben Sherman', 'sherm@mit.edu', TRUE))

table sport : { SportName : string, MinPlayers : int }
  PRIMARY KEY SportName

(*
val sportShow : {SportName : string} = 
  mkShow (fn r => r.SportName)
*)

table league : { LeagueName : string, Comment : string }

table sport_league : {SportName : string, LeagueName : string }
 (* CONSTRAINT Sport FOREIGN KEY SportName REFERENCES sport(SportName) ON UPDATE CASCADE,
  CONSTRAINT League FOREIGN KEY LeagueName REFERENCES league(LeagueName) ON UPDATE CASCADE *)

table participant : { Email : string, SportName : string, LeagueName : string, PreferredLeague : string, Captain : bool, Comments : string }
  (* CONSTRAINT Sport_League FOREIGN KEY (SportName, LeagueName) REFERENCES sport_league(SportName, LeagueName) ON UPDATE CASCADE,
  CONSTRAINT User FOREIGN KEY Email REFERENCES user(Email)
  *)

(* The real app uses client certificates, but here we'll do cookies for convenience. *)
cookie localC : string

(* Find the common name of the authenticated user (via SSL certificates),
 * remembering this person in the DB, if successful. *)
val auth =
    lo <- getCookie localC;
    case lo of
        None => error <xml>You haven't set the cookie with your name.</xml>
      | Some r => return r

val requireAuth = Monad.ignore auth

(* Fail if not authenticated as an admin. *)
val amAdmin =
    u <- auth;
    oneRowE1 (SELECT COUNT( * ) > 0
              FROM user
              WHERE user.Email = {[u]}
                AND user.IsAdmin)

val requireAdmin =
    isAdmin <- amAdmin;
    if isAdmin then
        return ()
    else
        error <xml>Access denied</xml>

val amUser = user <- auth; return (Some {Email = user})

(*
val rlabels = {RestaurantName = "Name",
               Neighborhood = "Neighborhood",
               Genre = "Genre",
               Url = "URL"}
*)

structure EditUsers = EditGrid.Make(struct
                                        con key = [Email = _]
                                        val tab = user
                                        val labels = {Email = "Email",
                                                      HumanName = "Name",
                                                      IsAdmin = "Admin?"}
                                        val authorized = amAdmin
                                    end)

structure EditSports = EditGrid.Make(struct
                                        con key = [SportName = _]
                                        val tab = sport
                                        val labels = {SportName = "Sport name",
                                                      MinPlayers = "Min # of players"}
                                        val authorized = amAdmin
                                    end)

(*
structure EditRestaurants = EditGrid.Make(struct
                                              con key = [RestaurantName = _, Neighborhood = _]
                                              val tab = restaurant
                                              val labels = rlabels
                                              val authorized = amAdmin
                                          end)
*)

val explainTime =
    Ui.h4 <xml>Your vote count is the number of people who would go, including you and your guests.</xml>

(*
val main =
    user <- auth;
    key <- return {Email = user};
    tm <- now;

    restaurants <- queryX1 (SELECT restaurant.RestaurantName, restaurant.Neighborhood
                            FROM restaurant
                            ORDER BY restaurant.RestaurantName, restaurant.Neighborhood)
                   (fn r => <xml><coption value={r.RestaurantName ^ "^"
                                                 ^ r.Neighborhood}>{[r.RestaurantName]}
                     {case r.Neighborhood of
                          "" => <xml/>
                        | s => <xml>({[s]})</xml>}</coption></xml>);

    whichT <- source "";
    whichR <- source "";
    whichTP <- source "";
    whichRP <- source "";
*)

val admin =
    requireAdmin;

    Theme.tabbed "Admin"
              ((Some "Users",
                EditUsers.ui),
               (Some "Sports",
                EditSports.ui))

fun setIt v =
    setCookie localC {Value = v,
                      Expires = None,
                      Secure = False}

val cookieSetup =
    sc <- source "";

    Theme.tabbed "Cookie Setup"
    {1 = (Some "Set Cookie",
      Ui.const <xml>
        <ctextbox source={sc}/>
        <button value="Set" onclick={fn _ => v <- get sc; rpc (setIt v)}/>
        </xml>)}

(* Dummy page to keep Ur/Web from garbage-collecting handlers within modules *)
val index = return <xml><body>
  <li><a link={cookieSetup}>Cookie set-up</a></li>
  <li><a link={admin}>Admin</a></li>
</body></xml>
