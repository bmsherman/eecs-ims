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

con sportSchema = { SportName : string, Leagues : string, MinPlayers : int }

table sport : { SportName : string, Leagues : string, MinPlayers : int }
  PRIMARY KEY SportName

(*
val sportShow : {SportName : string} = 
  mkShow (fn r => r.SportName)
*)

table league : { LeagueName : string, Comment : string }

table participant : { Email : string, SportName : string, PreferredLeague : string, Captain : bool, Comments : string }
  (* CONSTRAINT Sport FOREIGN KEY SportName REFERENCES sport(SportName) ON UPDATE CASCADE,
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
      | Some r => 
         inSystem <- oneRowE1 (SELECT COUNT( * ) > 0 FROM user WHERE user.Email = {[r]});
         if inSystem
           then return r
           else dml (INSERT INTO user(HumanName, Email, IsAdmin)
                     VALUES ({[r]}, {[r]}, FALSE));
                return r

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
                                                      Leagues = "Avail. leagues",
                                                      MinPlayers = "Min # of players"}
                                        val authorized = amAdmin
                                    end)

structure EditLeagues = EditGrid.Make(struct
                                        con key = [LeagueName = _]
                                        val tab = league
                                        val labels = {LeagueName = "League name",
                                                      Comment = "Comment"}
                                        val authorized = amAdmin
                                    end)


val explainTime =
    Ui.h4 <xml>Your vote count is the number of people who would go, including you and your guests.</xml>

fun sequenc [a] (xs : list (transaction a)) : transaction (list a) = 
     case xs of
        [] => return []
      | y :: ys => y' <- y;
                   ys' <- sequenc ys;
                   return (y' :: ys')

fun forM [a] [b] (xs : list a) (f : a -> transaction b) : transaction (list b) =
       sequenc (List.mp f xs)

fun signUp sprtN = 
  userE <- auth;
  dml (INSERT INTO participant(Email, SportName, PreferredLeague, Captain, Comments)
       VALUES ({[userE]}, {[sprtN]}, 'Any', FALSE, ''))

fun unSignup sprtN = 
  userE <- auth;
  dml (DELETE FROM participant
       WHERE Email = {[userE]} AND SportName = {[sprtN]})

type partType = {Email : string, SportName : string, Comments : string, PreferredLeague : string, HumanName : string, Captain : bool}

fun updateParticipant (r : partType) (newComm : string) (newPleague : string) (newCapt : bool) =
     dml (UPDATE participant
         SET Comments = {[newComm]}, PreferredLeague = {[newPleague]}, Captain = {[newCapt]}
         WHERE Email = {[r.Email]} AND SportName = {[r.SportName]}
         )

fun editParticipant userE (r : partType) = if userE = r.Email
  then 
    comm <- source r.Comments;
    pleague <- source r.PreferredLeague;
    capt <- source r.Captain;
    return <xml><li>{[r.HumanName]}, Preferred league: <ctextbox source={pleague}/>
                        , Comment: <ctextbox source={comm}/>
                        , Willing to captain?: <ccheckbox source={capt}/>
         <button value="Save" onclick={fn _ => 
           newComm <- get comm;
           newPleague <- get pleague;
           newCapt <- get capt;
           rpc (updateParticipant r newComm newPleague newCapt)
         }/></li></xml>
  else
    return <xml><li>{[r.HumanName
      ^ (if r.Captain then " (willing to captain)" else "")
      ^ (if r.PreferredLeague = "" then "" else " (preferred league: " ^ r.PreferredLeague ^ ")")
      ^ (if r.Comments = "" then "" else " (comment: " ^ r.Comments ^ ")")]}</li></xml>

fun getAllSignedUp userE (sprtN : string) =
      signedUp <- queryL (SELECT (user.HumanName) AS HumanName
                   , (participant.Email) AS Email
                   , (participant.PreferredLeague) AS PreferredLeague
                   , (participant.Comments) AS Comments
                   , (participant.SportName) AS SportName
                   , (participant.Captain) AS Captain
           FROM participant, user
           WHERE participant.SportName = {[sprtN]}
           AND user.Email = participant.Email
           ORDER BY user.HumanName DESC);
      List.mapXM (editParticipant userE) signedUp

fun sportUser (userE : string) (sprt : {SportName : string, MinPlayers : int, Leagues : string}) = 
      sprtN <- return sprt.SportName;
      parts <- getAllSignedUp userE sprtN;
      sparts <- source parts;
      signedUp <- oneRowE1 (SELECT COUNT( * ) > 0 FROM participant
           WHERE participant.Email = {[userE]} AND participant.SportName = {[sprtN]});
      ssignedUp <- source signedUp;
      return <xml><h3>{[sprtN]}</h3>
        <p>Min num players: {[sprt.MinPlayers]}</p>
        <p>Leagues: {[sprt.Leagues]}</p>
        <p>Signed up:</p>
        <dyn signal ={ allUp <- signal sparts;
                       sgn <- signal ssignedUp;
          return <xml>
           <ol>{allUp}</ol>
               {case sgn of
          False => <xml>
                       <button class="btn btn-primary"
                               value="Sign up!"
                               onclick={fn _ => rpc (signUp sprtN);
                                                newUp <- rpc (getAllSignedUp userE sprtN);
                                                set sparts newUp;
                                                set ssignedUp True}/>
                       </xml>
        | True => <xml>
                       <button class="btn btn-primary"
                               value="Un-sign up"
                               onclick={fn _ => rpc (unSignup sprtN);
                                                newUp <- rpc (getAllSignedUp userE sprtN);
                                                set sparts newUp;
                                                set ssignedUp False}/>
                       </xml>
                     }</xml>}/>
            </xml>

val displayLeagues =
  queryX1 (SELECT * FROM league ORDER BY league.LeagueName)
     (fn r => <xml><p>{[r.LeagueName]} : {[r.Comment]}</p></xml>)

val main =
    userE <- auth;
    key <- return {Email = userE};

    leagues <- displayLeagues;

    sports <- queryL1 (SELECT * FROM sport ORDER BY sport.SportName DESC);

    display <- List.mapXM (sportUser userE) sports;

    Theme.simple "EECS intramurals signup" 
      (Ui.const <xml>
       <h3>Leagues</h3>
       {leagues}
       {display}</xml>)


val admin =
    requireAdmin;

    Theme.tabbed "Admin"
              ((Some "Users", EditUsers.ui),
               (Some "Sports", EditSports.ui),
               (Some "Leagues", EditLeagues.ui))

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
  <li><a link={main}>Main</a></li>
</body></xml>
