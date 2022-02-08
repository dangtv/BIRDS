(** Operations for a db-connection, such as fetching columns or printing the results
 *)

open Printf
open Postgresql
open Utils
open Expr

(** Extract response column-names and tuples to lists
 *  Returns: empty_query, tuples_ok or error messages in case of problems *)
let res_to_lst conn res =
    match res#status with
  | Empty_query -> ([],[])
  | Tuples_ok -> (res#get_fnames_lst, res#get_all_lst)
  | Bad_response -> failwith (sprintf "Bad response: %s\n" res#error)(*;conn#reset*)
  | Nonfatal_error -> failwith (sprintf "Non fatal error: %s\n" res#error)
  | Fatal_error -> failwith (sprintf "Fatal error: %s\n" res#error)
  | _             -> failwith "error: unexpected status"



(** catch answers from the db server and return a (cn,t) tuple,
 * where cn is a list with the column-names and t the tuples*)
let get_query_result (conn:Postgresql.connection) sql =
    res_to_lst conn (conn#exec sql)


(** Given a table's headers and columns calculates the
 * length of the longest element in each column*)
let max_col_length head rows =
    let get_max x cell = max x (String.length cell) in
    let max_length len row = List.map2 get_max len row in
    let head_l = List.map String.length head in
    List.fold_left max_length head_l rows


(** Calculates the table's horizontal separator: a line of '-' characters*)
let get_sep col_l =
    let extra = (List.length col_l)*3+1 in
    let row_l = List.fold_left (fun x y->x+y) extra col_l in
    String.make row_l '-'


(** Pritty-prints a query's result*)
let print_res (head,rows) =
    let col_l = max_col_length head rows in
    let sep = get_sep col_l in
    let print_cell c l =
        let pad = String.make (l-(String.length c)) ' ' in
        Printf.printf "| %s%s " c pad in
    let print_row row =
        List.iter2 print_cell row col_l;
        print_endline "|" in
    print_endline sep;
    print_row head;
    print_endline sep;
    List.iter print_row rows;
    print_endline sep;
    flush stdout


(** Execute the provided SQL and print its resulting table
 * through the stdin *)
let print_sql_res (conn:Postgresql.connection) sql =
    let res_t = get_query_result conn sql in
    print_endline "";
    print_res res_t


(** Execute the provided SQL of creating views, triggers,... and print its result through the stdin *)
let print_creating_sql (conn:Postgresql.connection) sql =
    let res = (conn#exec sql) in
    print_endline "";
    match res#status with
    | Command_ok -> print_endline "-- run SQL successfully"
    | _             -> failwith "error of executing SQL: unexpected status"


let type_of_postgrestype str = match str with
    | "smallint" -> Sint
    | "integer"	-> Sint
    | "bigint"	-> Sint
    | "decimal variable" -> Sreal
    | "decimal" -> Sreal
    | "numeric variable" -> Sreal
    | "numeric" -> Sreal
    | "real" -> Sreal
    | "double precision" -> Sreal
    | "smallserial"	-> Sint
    | "serial" -> Sint
    | "bigserial"	-> Sint
    | "boolean" -> Sbool
    | "text" -> Sstring
    | "date" -> Sstring
    | _ -> Sstring

(** Looks in the db for the description of the provided table-name
 *  and returns a corresponding AST (Expr.stt)
 *  @param conn as postgresql connection type for database used *)
let import_table_schema (conn:Postgresql.connection) tname =
    let sql = "SELECT column_name,data_type,ordinal_position FROM information_schema.columns
                WHERE table_name ='"^tname^"';" in
    let _,tuples = get_query_result conn sql in
    let ordpos t = int_of_string (List.nth t 2) in
    let stuples = List.sort (fun a b -> (ordpos a) - (ordpos b)) tuples in
    let tuple_to_var_and_type t =
        let col = (String.uppercase_ascii (List.hd t)) in
        let type_of_var = type_of_postgrestype (List.nth t 1) in
        col,type_of_var in
    let col_types = List.map tuple_to_var_and_type stuples in
    (tname, col_types)


(** Looks in the db for the description of all the tables and views (exluding materialized view)
 * and returns a symtable with their descriptions*)
let import_dbschema (conn:Postgresql.connection) (dbschema:string) =
    let sql = "SELECT table_name FROM information_schema.tables
                WHERE table_schema = '"^dbschema^"';" in
    let _,tuples = get_query_result conn sql in
    let t_names = List.flatten tuples in
    (* let symt:symtable = Hashtbl.create 100 in *)
    (* let insert_t_pred conn symt t_name =
        symt_insert symt (import_table_schema conn t_name) in *)
    (* List.map (insert_t_pred conn symt) t_names; *)
    (* symt *)
    List.map (import_table_schema conn) t_names;
