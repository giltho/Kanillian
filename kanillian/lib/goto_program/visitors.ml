class ['a] iter =
  object (self)
    method visit_binop ~(ctx : 'a) (_op : Ops.Binary.t) =
      let _ = ctx in
      ()

    method visit_selfop ~(ctx : 'a) (_op : Ops.Self.t) =
      let _ = ctx in
      ()

    method visit_unop ~(ctx : 'a) (_op : Ops.Unary.t) =
      let _ = ctx in
      ()

    method visit_location ~(ctx : 'a) (_loc : Location.t) =
      let _ = ctx in
      ()

    method visit_int_type ~(ctx : 'a) (_int_ty : IntType.t) =
      let _ = ctx in
      ()

    method visit_datatype_components ~(ctx : 'a) (c : Datatype_component.t) =
      match c with
      | Field { type_; _ } -> self#visit_type ~ctx type_
      | Padding _ -> ()

    method visit_type ~(ctx : 'a) (type_ : Type.t) =
      match type_ with
      | CInteger int_ty -> self#visit_int_type ~ctx int_ty
      | Array (t, _) | Pointer t -> self#visit_type ~ctx t
      | Code { params; return_type } ->
          List.iter
            (fun ({ type_; _ } : Param.t) -> self#visit_type ~ctx type_)
            params;
          self#visit_type ~ctx return_type
      | Struct { components; _ } | Union { components; _ } ->
          List.iter (self#visit_datatype_components ~ctx) components
      | Empty
      | Constructor
      | Bool
      | Float
      | Double
      | Signedbv _
      | Unsignedbv _
      | StructTag _
      | UnionTag _
      | IncompleteStruct _ -> ()

    method visit_expr_value ~(ctx : 'a) (ev : Expr.value) =
      match ev with
      | Array l | Struct l -> List.iter (self#visit_expr ~ctx) l
      | FunctionCall { func; args } ->
          self#visit_expr ~ctx func;
          List.iter (self#visit_expr ~ctx) args
      | BinOp { op; lhs; rhs } ->
          self#visit_binop ~ctx op;
          self#visit_expr ~ctx lhs;
          self#visit_expr ~ctx rhs
      | UnOp { op; e } ->
          self#visit_unop ~ctx op;
          self#visit_expr ~ctx e
      | ByteExtract { e; _ } | Dereference e | AddressOf e | TypeCast e ->
          self#visit_expr ~ctx e
      | Index { array; index } ->
          self#visit_expr ~ctx array;
          self#visit_expr ~ctx index
      | Member { lhs; _ } -> self#visit_expr ~ctx lhs
      | AddressOfSymbol _
      | Nondet
      | Symbol _
      | IntConstant _
      | CBoolConstant _
      | BoolConstant _
      | PointerConstant _
      | StringConstant _ -> ()

    method visit_expr ~(ctx : 'a) (e : Expr.t) =
      self#visit_location ~ctx e.location;
      self#visit_expr_value ~ctx e.value;
      self#visit_type ~ctx e.type_

    method visit_stmt_body ~(ctx : 'a) (body : Stmt.body) =
      match body with
      | Decl { lhs; value } ->
          self#visit_expr ~ctx lhs;
          Option.iter (self#visit_expr ~ctx) value
      | Assign { lhs; rhs } ->
          self#visit_expr ~ctx lhs;
          self#visit_expr ~ctx rhs
      | Assume { cond } | Assert { cond } -> self#visit_expr ~ctx cond
      | Label (_, l) | Block l -> List.iter (self#visit_stmt ~ctx) l
      | Expression e -> self#visit_expr ~ctx e
      | Return e -> Option.iter (self#visit_expr ~ctx) e
      | Switch { control; cases; default } ->
          self#visit_expr ~ctx control;
          List.iter
            (fun ({ case; sw_body; _ } : Stmt.switch_case) ->
              self#visit_expr ~ctx case;
              self#visit_stmt ~ctx sw_body)
            cases;
          Option.iter (self#visit_stmt ~ctx) default
      | Goto _ | Skip -> ()

    method visit_stmt ~(ctx : 'a) (stmt : Stmt.t) =
      self#visit_location ~ctx stmt.location;
      self#visit_stmt_body ~ctx stmt.body
  end

(** Same as list.map, but takes a reference and marks it as true
   if any element was changed *)
let map_mark_changed ~changed f l =
  List.map
    (fun x ->
      let new_x = f x in
      if new_x == x then x
      else
        let () = changed := true in
        new_x)
    l

let option_map_preserve f o =
  match o with
  | None -> o
  | Some x ->
      let new_x = f x in
      if x == new_x then o else Some new_x

class ['a] map =
  object (self)
    method visit_binop ~(ctx : 'a) (op : Ops.Binary.t) =
      let _ = ctx in
      op

    method visit_selfop ~(ctx : 'a) (op : Ops.Self.t) =
      let _ = ctx in
      op

    method visit_unop ~(ctx : 'a) (op : Ops.Unary.t) =
      let _ = ctx in
      op

    method visit_location ~(ctx : 'a) (loc : Location.t) =
      let _ = ctx in
      loc

    method visit_int_type ~(ctx : 'a) (int_ty : IntType.t) =
      let _ = ctx in
      int_ty

    method visit_datatype_components ~(ctx : 'a) (c : Datatype_component.t) =
      match c with
      | Field { type_; name } ->
          let new_type_ = self#visit_type ~ctx type_ in
          if new_type_ == type_ then c else Field { type_ = new_type_; name }
      | Padding _ -> c

    method visit_type ~(ctx : 'a) (type_ : Type.t) =
      match type_ with
      | CInteger int_ty ->
          let new_int_ty = self#visit_int_type ~ctx int_ty in
          if new_int_ty == int_ty then type_ else CInteger new_int_ty
      | Array (t, _) | Pointer t -> self#visit_type ~ctx t
      | Code { params; return_type } ->
          let changed = ref false in
          let new_params =
            map_mark_changed ~changed
              (fun ({ type_; _ } as p : Param.t) ->
                let new_type = self#visit_type ~ctx type_ in
                if new_type == type_ then p else { p with type_ = new_type })
              params
          in
          let new_return_type = self#visit_type ~ctx return_type in
          if (not !changed) && new_return_type == return_type then type_
          else Code { params = new_params; return_type = new_return_type }
      | Struct { components; tag } ->
          let changed = ref false in
          let new_components =
            map_mark_changed ~changed
              (self#visit_datatype_components ~ctx)
              components
          in
          if not !changed then type_
          else Union { components = new_components; tag }
      | Union { components; tag } ->
          let changed = ref false in
          let new_components =
            map_mark_changed ~changed
              (self#visit_datatype_components ~ctx)
              components
          in
          if not !changed then type_
          else Union { components = new_components; tag }
      | Empty
      | Constructor
      | Bool
      | Float
      | Double
      | Signedbv _
      | Unsignedbv _
      | StructTag _
      | UnionTag _
      | IncompleteStruct _ -> type_

    method visit_expr_value ~(ctx : 'a) (ev : Expr.value) =
      match ev with
      | Array l ->
          let changed = ref false in
          let new_elems = map_mark_changed ~changed (self#visit_expr ~ctx) l in
          if not !changed then ev else Array new_elems
      | Struct l ->
          let changed = ref false in
          let new_elems = map_mark_changed ~changed (self#visit_expr ~ctx) l in
          if not !changed then ev else Struct new_elems
      | FunctionCall { func; args } ->
          let new_func = self#visit_expr ~ctx func in
          let changed = ref false in
          let new_args =
            map_mark_changed ~changed (self#visit_expr ~ctx) args
          in
          if (not !changed) && new_func == func then ev
          else FunctionCall { func = new_func; args = new_args }
      | BinOp { op; lhs; rhs } ->
          let new_op = self#visit_binop ~ctx op in
          let new_lhs = self#visit_expr ~ctx lhs in
          let new_rhs = self#visit_expr ~ctx rhs in
          if new_op == op && new_lhs == lhs && new_rhs == rhs then ev
          else BinOp { op = new_op; lhs = new_lhs; rhs = new_rhs }
      | UnOp { op; e } ->
          let new_op = self#visit_unop ~ctx op in
          let new_e = self#visit_expr ~ctx e in
          if new_op == op && new_e == e then ev
          else UnOp { op = new_op; e = new_e }
      | ByteExtract { e; offset } ->
          let new_e = self#visit_expr ~ctx e in
          if new_e == e then ev else ByteExtract { e = new_e; offset }
      | Dereference e ->
          let new_e = self#visit_expr ~ctx e in
          if new_e == e then ev else Dereference new_e
      | AddressOf e ->
          let new_e = self#visit_expr ~ctx e in
          if new_e == e then ev else AddressOf new_e
      | TypeCast e ->
          let new_e = self#visit_expr ~ctx e in
          if new_e == e then ev else TypeCast new_e
      | Index { array; index } ->
          let new_array = self#visit_expr ~ctx array in
          let new_index = self#visit_expr ~ctx index in
          if new_array == array && new_index == index then ev
          else Index { array = new_array; index = new_index }
      | Member { lhs; field } ->
          let new_lhs = self#visit_expr ~ctx lhs in
          if new_lhs == lhs then ev else Member { lhs = new_lhs; field }
      | AddressOfSymbol _
      | Nondet
      | Symbol _
      | IntConstant _
      | CBoolConstant _
      | BoolConstant _
      | PointerConstant _
      | StringConstant _ -> ev

    method visit_expr ~(ctx : 'a) (e : Expr.t) =
      let new_value = self#visit_expr_value ~ctx e.value in
      let new_location = self#visit_location ~ctx e.location in

      let new_type = self#visit_type ~ctx e.type_ in
      if
        new_value == e.value && new_location == e.location
        && new_type == e.type_
      then e
      else { value = new_value; location = new_location; type_ = new_type }

    method visit_stmt_body ~(ctx : 'a) (body : Stmt.body) =
      match body with
      | Decl { lhs; value } ->
          let new_lhs = self#visit_expr ~ctx lhs in
          let new_value = option_map_preserve (self#visit_expr ~ctx) value in
          if new_lhs == lhs && new_value == value then body
          else Decl { lhs = new_lhs; value = new_value }
      | Assign { lhs; rhs } ->
          let new_lhs = self#visit_expr ~ctx lhs in
          let new_rhs = self#visit_expr ~ctx rhs in
          if new_lhs == lhs && new_rhs == rhs then body
          else Assign { lhs = new_lhs; rhs = new_rhs }
      | Assume { cond } ->
          let new_cond = self#visit_expr ~ctx cond in
          if new_cond == cond then body else Assume { cond = new_cond }
      | Assert { cond } ->
          let new_cond = self#visit_expr ~ctx cond in
          if new_cond == cond then body else Assert { cond = new_cond }
      | Label (label, l) ->
          let changed = ref false in
          let new_l = map_mark_changed ~changed (self#visit_stmt ~ctx) l in
          if not !changed then body else Label (label, new_l)
      | Block l ->
          let changed = ref false in
          let new_l = map_mark_changed ~changed (self#visit_stmt ~ctx) l in
          if not !changed then body else Block new_l
      | Expression e ->
          let new_e = self#visit_expr ~ctx e in
          if new_e == e then body else Expression new_e
      | Return e ->
          let new_e = option_map_preserve (self#visit_expr ~ctx) e in
          if new_e == e then body else Return new_e
      | Switch { control; cases; default } ->
          let new_control = self#visit_expr ~ctx control in
          let changed = ref false in
          let new_cases =
            map_mark_changed ~changed
              (fun ({ case; sw_body } as sw_case : Stmt.switch_case) ->
                let new_case = self#visit_expr ~ctx case in
                let new_body = self#visit_stmt ~ctx sw_body in
                if new_case == case && new_body == sw_body then sw_case
                else { case = new_case; sw_body = new_body })
              cases
          in
          let new_default =
            option_map_preserve (self#visit_stmt ~ctx) default
          in
          if
            new_control == control && new_cases == cases
            && new_default == default
          then body
          else
            Switch
              {
                control = new_control;
                cases = new_cases;
                default = new_default;
              }
      | Goto _ | Skip -> body

    method visit_stmt ~(ctx : 'a) (stmt : Stmt.t) =
      let new_body = self#visit_stmt_body ~ctx stmt.body in
      let new_location = self#visit_location ~ctx stmt.location in
      if new_body == stmt.body && new_location == stmt.location then stmt
      else { body = new_body; location = new_location }
  end