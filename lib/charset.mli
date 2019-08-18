(** A module providing efficient character sets. *)

(** {2 Type} *)

(** The abstract type for a character set. *)
type charset

(** Synonym of [charset]. *)
type t = charset

(** {2 Charset construction} *)

(** The empty character set. *)
val empty : charset

(** The full character set. *)
val full : charset

(** [singleton c] returns a charset containing only [c]. *)
val singleton : char -> charset

(** [range cmin cmax] returns the charset containing all the  characters
    between [cmin] and [cmax]. *)
val range : char -> char -> charset

(** [from_string s] returns the charset corresponding to the description
    string [s],  which may contain standalone characters (different from
    ['-'], which is only allowed as first character) or ranges. They are
    build of start and end characters, separated by ['-']. An example of
    a valid description is ["-_a-zA-Z0-9"]. Note that [Invalid_argument]
    is raised in case of ill-formed description. *)
val from_string : string -> charset

(** [union cs1 cs2] builds a new charset that contins the union  of  the
    characters of [cs1] and [cs2]. *)
val union : charset -> charset -> charset

(** [complement cs] returns a new charset containing exactly  characters
    that are not in [cs]. *)
val complement : charset -> charset

(** [add cs c] returns a new charset containing the characters  of  [cs]
    and the character [c]. *)
val add : charset -> char -> charset

(** [del cs c] returns a new charset containing the characters  of  [cs]
    but not the character [c]. *)
val del : charset -> char -> charset

(** {2 Membership test} *)

(** [mem cs c] tests whether the charset [cs] contains [c]. *)
val mem : charset -> char -> bool

(** {2 Printing and string representation} *)

(** [pp ff cs] prints the charset [cs] to output formatter [ff]. Compact
    format is used for printing: ranges, full and empty charsets are not
    given in full, but abbreviated. *)
val pp : Format.formatter -> charset -> unit

(** [pp_full ff cs] is similar to [pp ff cs], but it does not abbreviate
    ranges, full and empty charsets. *)
val pp_full : Format.formatter -> charset -> unit

(** [show oc cs] builds a string representing the charset [cs] using the
    same compact format as [print]. *)
val show : charset -> string

(** [show_full oc cs] is the same as [show oc cs] but it  does  not  use
    abreviations (i.e. all characters appear). *)
val show_full : charset -> string

(** {2 Manipulating charsets imperatively} *)

(** [copy cs] make a copy of the charset [cs]. *)
val copy : charset -> charset

(** [addq cs c] adds the character [c] to the charset [cs].  Users  must
    be particularly careful when using this function. In particular,  it
    should not be used directly on [empty], [full] or the result of  the
    [singleton] function as it would change their value permanently.  It
    is advisable to prefer the use of [add] or to work on a [copy]. *)
val addq : charset -> char -> unit

(** [delq cs c] deletes the character [c] from the charset [cs]. Similar
    recomendatiosn as for [addq] apply. *)
val delq : charset -> char -> unit

(** {2 Comparison and equality test} *)

(** [compare cs1 cs2] compares the charsets [cs1] and [cs2] according to
    some (unspecified) total order. *)
val compare : charset -> charset -> int

(** [equal cs1 cs2] tests the equality of charsets [cs1] and [cs2]. *)
val equal : charset -> charset -> bool
