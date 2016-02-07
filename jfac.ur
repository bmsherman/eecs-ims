(* Organizing dinners for the MIT EECS junior faculty *)

open Bootstrap3
structure Theme = Ui.Make(Default)

table user : { Kerberos : string, HumanName : string, Email : string, IsAdmin : bool }
  PRIMARY KEY Kerberos

val userShow : show {HumanName : string} = mkShow (fn r => r.HumanName)
val userRead : read {HumanName : string} = mkRead' (fn s => Some {HumanName = s}) "user"

con sportSchema = { SportName : string, Leagues : string, MinPlayers : int }

table sport : { SportName : string, Leagues : string, MinPlayers : int }
  PRIMARY KEY SportName

table league : { LeagueName : string, Comment : string }

(* Bootstrap the database with an initial admin user. *)
task initialize = fn () =>
  anyUsers <- oneRowE1 (SELECT COUNT( * ) > 0
                        FROM user);
  if anyUsers then
      return ()
  else
      dml (INSERT INTO user(Kerberos, HumanName, Email, IsAdmin)
           VALUES ('sherm@MIT.EDU', 'Ben Sherman', 'sherm@mit.edu', TRUE));
      dml (INSERT INTO league(LeagueName, Comment)
           VALUES ('A', 'Experienced players who may have been playing together for some time (has playoffs)'));
      dml (INSERT INTO league(LeagueName, Comment)
           VALUES ('B', 'Majority of team has played the sport but not necessarily on a formal team (has playoffs)'));
      dml (INSERT INTO league(LeagueName, Comment)
           VALUES ('C', 'A casual league for those who have never played or wish to play leisurely (no playoffs)'));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Air Pistol', 'B', 1));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Billiards', 'A, B, C', 2));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Foosball', 'A, B', 3));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Indoor Soccer', 'A 7v7 reffed, A 7v7 unreffed, B 7v7 reffed, B 7v7 unreffed, B 5v5, B 5v5 coed, C 5v5', 5));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Kickball', 'A, B, C coed', 3));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Table Tennis', 'A, B, C', 5));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Team Tennis', 'A, B, C', 5));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Ultimate Frisbee', 'A, B, C, C coed', 7));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Unihoc', 'B, C', 5));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Volleyball', 'A, B, B coed, C', 6));
      dml (INSERT INTO sport(SportName, Leagues, MinPlayers)
           VALUES ('Waterpolo', 'A, B, C', 7))


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
         <button class="btn btn-primary" value="Save" onclick={fn _ => update} /></li></xml>
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
      listxml <- List.mapXM (editParticipant userK) signedUp;
      return { NumUp = List.length signedUp, XML = listxml }

fun sportUser (userK : string) (sprt : {SportName : string, MinPlayers : int, Leagues : string}) = 
      sprtN <- return sprt.SportName;
      parts <- getAllSignedUp userK sprtN;
      sparts <- source parts;
      signedUp <- oneRowE1 (SELECT COUNT( * ) > 0 FROM participant
           WHERE participant.Kerberos = {[userK]} AND participant.SportName = {[sprtN]});
      ssignedUp <- source signedUp;
      return <xml><div class="panel panel-default"><div class="panel-heading">{[sprtN]}</div>
        <div class="panel-body">
        <p>Leagues: {[sprt.Leagues]}</p>
        <dyn signal ={ allUp <- signal sparts;
                       sgn <- signal ssignedUp;
          return <xml>
           <p class={if allUp.NumUp >= sprt.MinPlayers 
                       then Bootstrap3.text_success 
                       else (if allUp.NumUp + 2 >= sprt.MinPlayers then Bootstrap3.text_warning else Bootstrap3.text_danger)}>{[allUp.NumUp]} signed up / {[sprt.MinPlayers]} minimum:</p>
           <ol>{allUp.XML}</ol>
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
            </div></div></xml>

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
