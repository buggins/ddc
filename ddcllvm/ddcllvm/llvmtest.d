module llvmtest;

import std.conv : to;
import std.stdio : writefln, writeln, readln;

import llvm.c;

import llvm.util.memory;

LLVMValueRef createVarargFunc(LLVMModuleRef mod, string name) {
    //LLVMAvailableExternallyLinkage
    auto paramtype = LLVMPointerType(LLVMInt8Type(), 0);
    //LLVMSetConstant(paramtype, 1);
    auto param_types = [ paramtype ];
    auto ret_type = LLVMFunctionType(LLVMInt32Type(), param_types.ptr, 1, 0);
    auto res = LLVMAddFunction(mod, name.toCString, ret_type);
    LLVMSetLinkage(res, LLVMExternalLinkage);//LLVMAvailableExternallyLinkage
	LLVMSetFunctionCallConv(res, LLVMCCallConv);
    //LLVMAddFunctionAttr(res, LLVMAvailableExternallyLinkage); //LLVMAvailableExternallyLinkage
    //LLVMAddFunctionAttr(res, LLVMExternalLinkage); //LLVMAvailableExternallyLinkage
    return res;
}

LLVMValueRef createNoargIntFunc(LLVMModuleRef mod, string name) {
    //LLVMAvailableExternallyLinkage
    auto ret_type = LLVMFunctionType(LLVMInt32Type(), null, 0, 0);
    auto res = LLVMAddFunction(mod, name.toCString, ret_type);
    LLVMSetLinkage(res, LLVMExternalLinkage);//LLVMAvailableExternallyLinkage
	LLVMSetFunctionCallConv(res, LLVMCCallConv);
    //LLVMAddFunctionAttr(res, LLVMAvailableExternallyLinkage); //LLVMAvailableExternallyLinkage
    //LLVMAddFunctionAttr(res, LLVMExternalLinkage); //LLVMAvailableExternallyLinkage
    return res;
}

LLVMValueRef llvmGenLocalStringVar(LLVMModuleRef mod, string data)
{
    LLVMValueRef glob = LLVMAddGlobal(mod, LLVMArrayType(LLVMInt8Type(), cast(int)data.length), "string");

    // set as internal linkage and constant
    LLVMSetLinkage(glob, LLVMInternalLinkage);
    LLVMSetGlobalConstant(glob, 1);

    // Initialize with string:
    //LLVMSetInitializer(glob, LLVMString(data.toCString, cast(int)data.length, 0));
    LLVMSetInitializer(glob, LLVMConstString(data.toCString, cast(int)data.length, 0));


    return glob;
}

/*
    //LLVMAvailableExternallyLinkage
    LLVMValueRef printfFunc = createVarargFunc(mod, "printf");
    LLVMValueRef dummyFunc = createNoargIntFunc(mod, "dummy");
    //LLVMValueRef msg = llvmGenLocalStringVar(mod, "Hello World!\n");

    auto builder = LLVMCreateBuilder();

    auto param_types = [ LLVMInt32Type(), LLVMInt32Type() ];
    auto ret_type = LLVMFunctionType(LLVMInt32Type(), param_types.ptr, 2, 0);
    auto sum = LLVMAddFunction(mod, "sum".toCString, ret_type);
    //LLVMAddFunctionAttr(sum, LLVMExternalLinkage); //LLVMAvailableExternallyLinkage
    auto entry = LLVMAppendBasicBlock(sum, "entry".toCString);
    LLVMPositionBuilderAtEnd(builder, entry);

    auto index_0 = [LLVMConstInt(LLVMInt32Type(), 0, cast(LLVMBool) false)];
    //auto msg_ptr = LLVMBuildGEP(builder, msg, index_0.ptr, 1, "msg.ptr");
    auto msg_ptr = LLVMBuildGlobalStringPtr(builder, "Hello World".toCString(), "msg_ptr");
    auto printfParams = [msg_ptr];
	auto call_printf = LLVMBuildCall(builder, printfFunc, printfParams.ptr, 1, "printf()".toCString());


	//auto call_dummy = LLVMBuildCall(builder, dummyFunc, null, 0, "dummy()".toCString());
    auto param1 = LLVMGetParam(sum, 0);
    //auto param2 = LLVMGetParam(sum, 1);
    //auto tmp = LLVMBuildAdd(builder, param1, param2, "tmp".toCString);
    //auto tmp = LLVMBuildAdd(builder, param1, call_dummy, "tmp".toCString);
    auto tmp = LLVMBuildAdd(builder, param1, call_printf, "tmp".toCString);
    LLVMBuildRet(builder, tmp);
    LLVMExecutionEngineRef engine;
    //LLVMLinkInJIT();
    LLVMInitializeNativeTarget();
    if (LLVMCreateExecutionEngineForModule(&engine, mod, &error) != 0) {
        writeln("failed to create execution engine");
        return 1;
    }
    if (error) {
        writeln("error: ", error);
        LLVMDisposeMessage(error);
        return 2;
    }

    int x = args.length > 1 ? to!int(args[1]) : 5;
    int y = args.length > 2 ? to!int(args[2]) : 7;

    auto fargs = [
        LLVMCreateGenericValueOfInt(LLVMInt32Type(), x, 0),
        LLVMCreateGenericValueOfInt(LLVMInt32Type(), y, 0)
    ];

    auto res = LLVMRunFunction(engine, main, 2, fargs.ptr);
    if (!res) {
        writeln("result is null");
        return 5;
    }
    int n = cast(int)LLVMGenericValueToInt(res, 0);

    writeln("result=", n);


    LLVMDisposeBuilder(builder);
    LLVMDisposeExecutionEngine(engine);
*/

LLVMValueRef genMain(LLVMModuleRef mod) {
    auto builder = LLVMCreateBuilder();
    LLVMValueRef printfFunc = createVarargFunc(mod, "printf");

    auto ret_type = LLVMFunctionType(LLVMInt32Type(), null, 0, 0);
    auto res = LLVMAddFunction(mod, "main".toCString, ret_type);
	LLVMSetFunctionCallConv(res, LLVMCCallConv);

    auto entry = LLVMAppendBasicBlock(res, "entry".toCString);
    LLVMPositionBuilderAtEnd(builder, entry);

    auto msg_ptr = LLVMBuildGlobalStringPtr(builder, "Hello World".toCString(), "msg_ptr");
    auto printfParams = [msg_ptr];
	auto call_printf = LLVMBuildCall(builder, printfFunc, printfParams.ptr, 1, "printf()".toCString());

    LLVMBuildRet(builder, call_printf);

    LLVMDisposeBuilder(builder);
    return res;
}

int fibo(string[]args) {
	char* error;

    //LLVMLinkInJIT();
    LLVMInitializeNativeTarget();
    auto mod = LLVMModuleCreateWithName("my_module".toCString);

    LLVMValueRef mainFunc = genMain(mod);

    LLVMVerifyModule(mod, LLVMAbortProcessAction, &error);
    if (error)
        LLVMDisposeMessage(error);
    error = null;

    // Write out bitcode to file
    if (LLVMWriteBitcodeToFile(mod, "sum.bc") != 0) {
        writeln("error writing bitcode to file, skipping\n");
    }

    readln();
    return 0;
}

int fiboFull(string[] args) {
	char* error;


	LLVMInitializeNativeTarget();
	auto _module = LLVMModuleCreateWithName("fibonacci".toCString());
	auto f_args = [ LLVMInt32Type() ];
	auto f = LLVMAddFunction(
		_module,
		"fib",
		LLVMFunctionType(LLVMInt32Type(), f_args.ptr, 1, cast(LLVMBool) false));
	LLVMSetFunctionCallConv(f, LLVMCCallConv);
	
	auto n = LLVMGetParam(f, 0);
	
	auto entry = LLVMAppendBasicBlock(f, "entry".toCString());
	auto case_base0 = LLVMAppendBasicBlock(f, "case_base0".toCString());
	auto case_base1 = LLVMAppendBasicBlock(f, "case_base1".toCString());
	auto case_default = LLVMAppendBasicBlock(f, "case_default".toCString());
	auto end = LLVMAppendBasicBlock(f, "end".toCString());
	auto builder = LLVMCreateBuilder();
	
	/+ Entry basic block +/
	LLVMPositionBuilderAtEnd(builder, entry);
	auto Switch = LLVMBuildSwitch(
		builder,
		n,
		case_default,
		2);
	LLVMAddCase(Switch, LLVMConstInt(LLVMInt32Type(), 0, cast(LLVMBool) false), case_base0);
	LLVMAddCase(Switch, LLVMConstInt(LLVMInt32Type(), 1, cast(LLVMBool) false), case_base1);

	/+ Basic block for n = 0: fib(n) = 0 +/
	LLVMPositionBuilderAtEnd(builder, case_base0);
	auto res_base0 = LLVMConstInt(LLVMInt32Type(), 0, cast(LLVMBool) false);
	LLVMBuildBr(builder, end);
	
	/+ Basic block for n = 1: fib(n) = 1 +/
	LLVMPositionBuilderAtEnd(builder, case_base1);
	auto res_base1 = LLVMConstInt(LLVMInt32Type(), 1, cast(LLVMBool) false);
	LLVMBuildBr(builder, end);
	
	/+ Basic block for n > 1: fib(n) = fib(n - 1) + fib(n - 2) +/
	LLVMPositionBuilderAtEnd(builder, case_default);

	auto n_minus_1 = LLVMBuildSub(
		builder,
		n,
		LLVMConstInt(LLVMInt32Type(), 1, cast(LLVMBool) false),
		"n - 1".toCString());
	auto call_f_1_args = [ n_minus_1 ];
	auto call_f_1 = LLVMBuildCall(builder, f, call_f_1_args.ptr, 1, "fib(n - 1)".toCString());
	
	auto n_minus_2 = LLVMBuildSub(
		builder,
		n,
		LLVMConstInt(LLVMInt32Type(), 2, cast(LLVMBool) false),
		"n - 2".toCString());
	auto call_f_2_args = [ n_minus_2 ];
	auto call_f_2 = LLVMBuildCall(builder, f, call_f_2_args.ptr, 1, "fib(n - 2)".toCString());
	
	auto res_default = LLVMBuildAdd(builder, call_f_1, call_f_2, "fib(n - 1) + fib(n - 2)".toCString());
	LLVMBuildBr(builder, end);
	
	/+ Basic block for collecting the result +/
	LLVMPositionBuilderAtEnd(builder, end);
	auto res = LLVMBuildPhi(builder, LLVMInt32Type(), "result".toCString());
	auto phi_vals = [ res_base0, res_base1, res_default ];
	auto phi_blocks = [ case_base0, case_base1, case_default ];
	LLVMAddIncoming(res, phi_vals.ptr, phi_blocks.ptr, 3);
	LLVMBuildRet(builder, res);
	
	LLVMVerifyModule(_module, LLVMAbortProcessAction, &error);
	LLVMDisposeMessage(error);
	
	LLVMExecutionEngineRef engine;
	error = null;
	
	version(Windows)
	{
		/+ On Windows, we can only use the old JIT for now +/
		LLVMCreateJITCompilerForModule(&engine, _module, 2, &error);
	}
	else
	{
		// Ran into some issues under Arch, so comment the new MVJIT out for now
		/+static if(LLVM_Version >= 3.3)
		{
			/+ On other systems we should be able to use the newer
			 + MCJIT instead - if we have a high enough LLVM version +/
			LLVMMCJITCompilerOptions options;
			LLVMInitializeMCJITCompilerOptions(&options, options.sizeof);

			LLVMCreateMCJITCompilerForModule(&engine, _module, &options, options.sizeof, &error);
		}
		else
		{+/
			LLVMCreateJITCompilerForModule(&engine, _module, 2, &error);
		//}
	}

	if(error !is null)
	{
		writefln("%s", error.fromCString());
		LLVMDisposeMessage(error);
		return 1;
	}
	
	auto pass = LLVMCreatePassManager();
	LLVMAddTargetData(LLVMGetExecutionEngineTargetData(engine), pass);
	LLVMAddConstantPropagationPass(pass);
	LLVMAddInstructionCombiningPass(pass);
	LLVMAddPromoteMemoryToRegisterPass(pass);
	LLVMAddGVNPass(pass);
	LLVMAddCFGSimplificationPass(pass);
	LLVMRunPassManager(pass, _module);
	
	writefln("The following module has been generated for the fibonacci series:\n");
	LLVMDumpModule(_module);
	
	writeln();
	
	int n_exec= 10;
	if(args.length > 1)
	{
		n_exec = to!int(args[1]);
	}
	else
	{
		writefln("; Argument for fib missing on command line, using default:  \"%d\"", n_exec);
	}
	
	auto exec_args = [ LLVMCreateGenericValueOfInt(LLVMInt32Type(), n_exec, cast(LLVMBool) 0) ];
	writefln("; Running (jit-compiled) fib(%d)...", n_exec);
	auto exec_res = LLVMRunFunction(engine, f, 1, exec_args.ptr);
	writefln("; fib(%d) = %d", n_exec, LLVMGenericValueToInt(exec_res, 0));
	
	LLVMDisposePassManager(pass);
	LLVMDisposeBuilder(builder);
	LLVMDisposeExecutionEngine(engine);
	return 0;	
}
