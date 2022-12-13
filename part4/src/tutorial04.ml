open Llvm

let add_target_triple triple llm =
  Llvm_X86.initialize ();
  let lltarget  = Llvm_target.Target.by_triple triple in
  let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
  let lldly     = Llvm_target.TargetMachine.data_layout llmachine in

  set_target_triple (Llvm_target.TargetMachine.triple llmachine) llm ;
  set_data_layout (Llvm_target.DataLayout.as_string lldly) llm ;
  Printf.printf "lltarget: %s\n" (Llvm_target.Target.name lltarget);
  Printf.printf "llmachine: %s\n" (Llvm_target.TargetMachine.triple llmachine);
  Printf.printf "lldly: %s\n" (Llvm_target.DataLayout.as_string lldly) ;
  ()


let _ =
  let llctx = global_context () in
  let llm = create_module llctx "mymodule" in

  add_target_triple "x86_64" llm ;
  let i8_t = i8_type llctx in
  let i32_t = i32_type llctx in
  let fty = function_type i32_t [| |] in

  let f = define_function "main" fty llm in
  let llbuilder = builder_at_end llctx (entry_block f) in

  let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
  let printf = declare_function "printf" printf_ty llm in
  (* let nounwind = create_enum_attr llctx "nounwind" 0L in  
  let nocapture = create_enum_attr llctx "nocapture" 0L in   *)
  let nounwind = attr_of_repr llctx (AttrRepr.Enum ((enum_attr_kind "nounwind"), 0L)) in  
  let nocapture = attr_of_repr llctx (AttrRepr.Enum ((enum_attr_kind "nocapture"), 0L)) in  
  add_function_attr printf nounwind AttrIndex.Function;
  add_function_attr printf nocapture (AttrIndex.Param 0);

  let s = build_global_stringptr "Hello, world!\n" "" llbuilder in
  (* try commenting these two lines and compare the result *)
  let zero = const_int i32_t 0 in
  let s = build_in_bounds_gep s [| zero |] "" llbuilder in

  let _ = build_call printf [| s |] "" llbuilder in

  let _ = build_ret (const_int i32_t 0) llbuilder in

  Llvm_analysis.assert_valid_module llm ;
  let _ =
    if Array.length Sys.argv > 1
    then Llvm_bitwriter.write_bitcode_file llm Sys.argv.(1) |> ignore
    else dump_module llm
  in
  ()
