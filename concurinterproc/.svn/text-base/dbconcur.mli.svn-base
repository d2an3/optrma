(** Duplicate and rename procedures in each thread *)

type readwrite = {
  mutable globalread : string Sette.t;
  mutable globalwrite : string Sette.t;
  mutable globalreadother : string Sette.t;
  mutable globalwriteother : string Sette.t;
}

val rename_program : 
  (unit,unit) Db.program -> (unit, readwrite) Db.program
