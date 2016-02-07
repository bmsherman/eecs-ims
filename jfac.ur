(* Organizing dinners for the MIT EECS junior faculty *)

open Bootstrap3
structure Theme = Ui.Make(Default)

table user : { Kerberos : string, HumanName : string, Email : string, IsAdmin : bool }
  PRIMARY KEY Kerberos

val userShow : show {HumanName : string} = mkShow (fn r => r.HumanName)
val userRead : read {HumanName : string} = mkRead' (fn s => Some {HumanName = s}) "user"

(* Bootstrap the database with an initial admin user. *)
task initialize = fn () =>
  anyUsers <- oneRowE1 (SELECT COUNT( * ) > 0
                        FROM user);
  if anyUsers then
      return ()
  else
      dml (INSERT INTO user(Kerberos, HumanName, Email, IsAdmin)
           VALUES ('sherm@MIT.EDU', 'Ben Sherman', 'sherm@mit.edu', TRUE))

con sportSchema = { SportName : string, Leagues : string, MinPlayers : int }

table sport : { SportName : string, Leagues : string, MinPlayers : int }
  PRIMARY KEY SportName

(*
val sportShow : {SportName : string} = 
  mkShow (fn r => r.SportName)
*)

table league : { LeagueName : string, Comment : string }

table participant : { Kerberos : string, SportName : string, PreferredLeague : string, Captain : bool, Comments : string }
  (* CONSTRAINT Sport FOREIGN KEY SportName REFERENCES sport(SportName) ON UPDATE CASCADE,
  CONSTRAINT User FOREIGN KEY Email REFERENCES user(Email)
  *)

type cert = {Email : string, CommonName : string}

(* The real app uses client certificates, but here we'll do cookies for convenience. *)
cookie localC : cert

fun validate (user : cert) : transaction cert = 
  case String.split user.Email #"@" of
       None => error <xml>Invalid e-mail address in certificate</xml>
     | Some (uname, dom) =>
          if dom <> "MIT.EDU" then
              error <xml>Certificate is not for an MIT e-mail address.</xml>
          else
              return user

(* Find the common name of the authenticated user (via SSL certificates),
 * remembering this person in the DB, if successful. *)
val auth : transaction string =
    lo <- getCookie localC;
    case lo of
        None => error <xml>You haven't set the cookie with your name.</xml>
      | Some r => 
         r' <- validate r;
         inSystem <- oneRowE1 (SELECT COUNT( * ) > 0 FROM user WHERE user.Kerberos = {[r'.Email]});
         if inSystem
           then return r'.Email
           else dml (INSERT INTO user(Kerberos, HumanName, Email, IsAdmin)
                     VALUES ({[r'.Email]}, {[r'.CommonName]}, {[r'.Email]}, FALSE));
                return r'.Email

val requireAuth = Monad.ignore auth

(* Fail if not authenticated as an admin. *)
val amAdmin =
    u <- auth;
    oneRowE1 (SELECT COUNT( * ) > 0
              FROM user
              WHERE user.Kerberos = {[u]}
                AND user.IsAdmin)

val requireAdmin =
    isAdmin <- amAdmin;
    if isAdmin then
        return ()
    else
        error <xml>Access denied</xml>

val amUser = user <- auth; return (Some {Keberos = user})

structure EditUsers = EditGrid.Make(struct
                                        con key = [Kerberos = _]
                                        val tab = user
                                        val labels = {Email = "Email",
                                                      HumanName = "Name",
                                                      Kerberos = "Kerberos",
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
  userK <- auth;
  dml (INSERT INTO participant(Kerberos, SportName, PreferredLeague, Captain, Comments)
       VALUES ({[userK]}, {[sprtN]}, 'Any', FALSE, ''))

fun unSignup sprtN = 
  userK <- auth;
  dml (DELETE FROM participant
       WHERE Kerberos = {[userK]} AND SportName = {[sprtN]})

type partType = {Kerberos : string, SportName : string, Comments : string, PreferredLeague : string, HumanName : string, Captain : bool}

fun updateParticipant (r : partType) (newComm : string) (newPleague : string) (newCapt : bool) =
     dml (UPDATE participant
         SET Comments = {[newComm]}, PreferredLeague = {[newPleague]}, Captain = {[newCapt]}
         WHERE Kerberos = {[r.Kerberos]} AND SportName = {[r.SportName]}
         )

fun editParticipant userK (r : partType) = if userK = r.Kerberos
  then 
    comm <- source r.Comments;
    pleague <- source r.PreferredLeague;
    capt <- source r.Captain;
    let val update =
           newComm <- get comm;
           newPleague <- get pleague;
           newCapt <- get capt;
           rpc (updateParticipant r newComm newPleague newCapt)
    in
      return <xml><li>{[r.HumanName]}
                        , Preferred league: <ctextbox onchange={update} source={pleague}/>
                        , Comment: <ctextbox onchange={update} source={comm}/>
                        , Willing to captain?: <ccheckbox onchange={update} source={capt}/>
         <button value="Save" onclick={fn _ => update} /></li></xml>
    end
  else
    return <xml><li>{[r.HumanName
      ^ (if r.Captain then " (willing to captain)" else "")
      ^ (if r.PreferredLeague = "" then "" else " (preferred league: " ^ r.PreferredLeague ^ ")")
      ^ (if r.Comments = "" then "" else " (comment: " ^ r.Comments ^ ")")]}</li></xml>

fun getAllSignedUp userK (sprtN : string) =
      signedUp <- queryL (SELECT (user.HumanName) AS HumanName
                   , (participant.Kerberos) AS Kerberos
                   , (participant.PreferredLeague) AS PreferredLeague
                   , (participant.Comments) AS Comments
                   , (participant.SportName) AS SportName
                   , (participant.Captain) AS Captain
           FROM participant, user
           WHERE participant.SportName = {[sprtN]}
           AND user.Kerberos = participant.Kerberos
           ORDER BY user.HumanName DESC);
      List.mapXM (editParticipant userK) signedUp

fun sportUser (userK : string) (sprt : {SportName : string, MinPlayers : int, Leagues : string}) = 
      sprtN <- return sprt.SportName;
      parts <- getAllSignedUp userK sprtN;
      sparts <- source parts;
      signedUp <- oneRowE1 (SELECT COUNT( * ) > 0 FROM participant
           WHERE participant.Kerberos = {[userK]} AND participant.SportName = {[sprtN]});
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
                                                newUp <- rpc (getAllSignedUp userK sprtN);
                                                set sparts newUp;
                                                set ssignedUp True}/>
                       </xml>
        | True => <xml>
                       <button class="btn btn-primary"
                               value="Un-sign up"
                               onclick={fn _ => rpc (unSignup sprtN);
                                                newUp <- rpc (getAllSignedUp userK sprtN);
                                                set sparts newUp;
                                                set ssignedUp False}/>
                       </xml>
                     }</xml>}/>
            </xml>

val displayLeagues =
  queryX1 (SELECT * FROM league ORDER BY league.LeagueName)
     (fn r => <xml><p>{[r.LeagueName]} : {[r.Comment]}</p></xml>)

val main =
    userK <- auth;
    key <- return {Kerberos = userK};

    leagues <- displayLeagues;

    sports <- queryL1 (SELECT * FROM sport ORDER BY sport.SportName DESC);

    display <- List.mapXM (sportUser userK) sports;

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
    scname <- source "";
    scemail <- source "";
    user <- ClientCert.user;

    Theme.tabbed "Cookie Setup"
    {1 = (Some "Set Cookie",
      Ui.const <xml>
        <ctextbox source={scname}/>
        <ctextbox source={scemail}/>
        <button value="Set" onclick={fn _ => name <- get scname; email <- get scemail; 
                                             rpc (setIt { Email = email, CommonName = name })}/>
        <p>
    {case user of
        None => <xml>No user</xml>
      | Some r => <xml>Email: {[r.Email]}, Name: {[r.CommonName]}</xml>
    }</p>
        </xml>)}

(* Dummy page to keep Ur/Web from garbage-collecting handlers within modules *)
val index = return <xml><body>
  <li><a link={cookieSetup}>Cookie set-up</a></li>
  <li><a link={admin}>Admin</a></li>
  <li><a link={main}>Main</a></li>
</body></xml>
