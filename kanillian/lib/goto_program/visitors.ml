class ['a] iter =
  object (self)
    method visit_binop ~(ctx : 'a) (op : Ops.Binary.t) = ()
    method visit_selfop ~(ctx : 'a) (op : Ops.Self.t) = ()
    method visit_unop ~(ctx : 'a) (op : Ops.Unary.t) = ()
    method visit_location ~(ctx : 'a) (loc : Location.t) = ()
    method visit_int_type ~(ctx : 'a) (int_ty : IntType.t) = ()

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
      | ByteExtract { e } | Dereference e | AddressOf e | TypeCast e ->
          self#visit_expr ~ctx e
      | Index { array; index } ->
          self#visit_expr ~ctx array;
          self#visit_expr ~ctx index
      | Member { lhs; _ } -> self#visit_expr ~ctx lhs
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
            (fun ({ case; body } : Stmt.switch_case) ->
              self#visit_expr ~ctx case;
              self#visit_stmt ~ctx body)
            cases;
          Option.iter (self#visit_stmt ~ctx) default
      | Goto _ | Skip -> ()

    method visit_stmt ~(ctx : 'a) (stmt : Stmt.t) =
      self#visit_location ~ctx stmt.location;
      self#visit_stmt_body ~ctx stmt.body
  end