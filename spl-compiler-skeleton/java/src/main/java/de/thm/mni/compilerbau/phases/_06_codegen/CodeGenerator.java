package de.thm.mni.compilerbau.phases._06_codegen;

import de.thm.mni.compilerbau.CommandLineOptions;
import de.thm.mni.compilerbau.absyn.*;
import de.thm.mni.compilerbau.table.SymbolTable;
import de.thm.mni.compilerbau.types.ArrayType;
import de.thm.mni.compilerbau.utils.NotImplemented;
import de.thm.mni.compilerbau.table.*;
import de.thm.mni.compilerbau.utils.SplError;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

/**
 * This class is used to generate the assembly code for the compiled program.
 * This code is emitted via the {@link CodePrinter} in the output field of this class.
 */
public class CodeGenerator  {
    final CommandLineOptions options;
    final CodePrinter output ;
    private int countOfLabels = 0;
    private int registerCounter = 8;
    Map<BinaryExpression.Operator, Instruction> instructionsCompare = new HashMap<>();
    Map<BinaryExpression.Operator, Instruction2> instructionASMD = new HashMap<>();
    interface Instruction {
        void execute(int reg1, int reg2, String label);
    }
    interface Instruction2 {
        void execute(int reg1, int reg2, int label);
    }



    /**
     * Initializes the code generator.
     *
     * @param options The command line options passed to the compiler
     * @param output  The PrintWriter to the output file.
     */
    public CodeGenerator(CommandLineOptions options, PrintWriter output) throws IOException {
        this.options = options;
        this.output = new CodePrinter(output);
        initMapCompare(instructionsCompare);
        initMap(instructionASMD);
//        printMapsContent();
    }
//    public void printMapsContent() {
//        System.out.println("=== instructionsCompare Map ===");
//        for (Map.Entry<BinaryExpression.Operator, Instruction> entry : instructionsCompare.entrySet()) {
//            BinaryExpression.Operator operator = entry.getKey();
//            Instruction instruction = entry.getValue();
//            System.out.println(operator + " -> " + instruction.getClass().getSimpleName());
//        }
//
//        System.out.println("\n=== instructionASMD Map ===");
//        for (Map.Entry<BinaryExpression.Operator, Instruction2> entry : instructionASMD.entrySet()) {
//            BinaryExpression.Operator operator = entry.getKey();
//            Instruction2 instruction = entry.getValue();
//            System.out.println(operator + " -> " + instruction.getClass().getSimpleName());
//        }
//    }

    public void generateCode(Program program, SymbolTable table) {
        assemblerProlog();

        program.definitions.stream().forEach(procDef -> {
            if(procDef instanceof ProcedureDefinition){
                generateCode((ProcedureDefinition ) procDef, table);
            }
        });
        //TODO (assignment 6): generate eco32 assembler code for the spl program

    }
    public void generateTheStartOfProc(ProcedureDefinition procedureDefinition, ProcedureEntry procedureEntry) {
        output.emitExport(procedureDefinition.name.toString());
        output.emitLabel(procedureDefinition.name.toString());
        output.emitInstruction("sub", new Register(29), new Register(29), procedureEntry.stackLayout.frameSize(), "allocate frame");
        output.emitInstruction("stw", new Register(25), new Register(29), procedureEntry.stackLayout.oldFramePointerOffset() , "save old frame pointer");
        output.emitInstruction("add", new Register(25), new Register(29), procedureEntry.stackLayout.frameSize(), "setup new frame pointer");
        output.emitInstruction("stw", new Register(31), new Register(25), procedureEntry.stackLayout.oldReturnAddressOffset(), "save return register");
    }

    public void generateTheEndOfProc(ProcedureEntry procedureEntry){
        output.emitInstruction("ldw", new Register(31), new Register(25),procedureEntry.stackLayout.oldReturnAddressOffset(), "restore return register");
        output.emitInstruction("ldw", new Register(25), new Register(29),procedureEntry.stackLayout.oldFramePointerOffset(), "restore old frame pointer");
        output.emitInstruction("add", new Register(29), new Register(29),procedureEntry.stackLayout.frameSize(), "release frame");
        output.emitInstruction("jr", new Register(31), "return");
    }
    public void generateCode(ProcedureDefinition procedureDefinition, SymbolTable table) {
        ProcedureEntry procedureEntry = (ProcedureEntry) table.lookup(procedureDefinition.name);
        generateTheStartOfProc(procedureDefinition, procedureEntry);
        procedureDefinition.body.forEach(stat ->  generateCode(stat, procedureEntry.localTable));
        generateTheEndOfProc(procedureEntry);
    }

    private void doNothing(){
    }
    public void generateCode(Statement statement, SymbolTable table) {
        switch (statement){
            case WhileStatement whileStatement ->{
                generateCode(whileStatement, table);
            }
            case IfStatement ifStatement ->{
                generateCode(ifStatement, table);
            }
            case EmptyStatement emptyStatement->{
                    doNothing();
            }
            case CompoundStatement compoundStatement->{
                generateCode(compoundStatement, table);
            }
            case AssignStatement assignStatement->{
                generateCode(assignStatement, table);
            }
            case CallStatement callStatement->{
                generateCode(callStatement, table);
            }
        }

    }
    private void initMapCompare(Map<BinaryExpression.Operator, Instruction> instructions) {
        instructions.put(BinaryExpression.Operator.NEQ, (reg1, reg2, label) -> output.emitInstruction("beq", new Register(reg1), new Register(reg2), label));
        instructions.put(BinaryExpression.Operator.GRE, (reg1, reg2, label) -> output.emitInstruction("blt", new Register(reg1), new Register(reg2), label));
        instructions.put(BinaryExpression.Operator.GRT, (reg1, reg2, label) -> output.emitInstruction("ble", new Register(reg1), new Register(reg2), label));
        instructions.put(BinaryExpression.Operator.LST, (reg1, reg2, label) -> output.emitInstruction("bge", new Register(reg1), new Register(reg2), label));
        instructions.put(BinaryExpression.Operator.LSE, (reg1, reg2, label) -> output.emitInstruction("bgt", new Register(reg1), new Register(reg2), label));
        instructions.put(BinaryExpression.Operator.EQU, (reg1, reg2, label) -> output.emitInstruction("bne", new Register(reg1), new Register(reg2), label));
    }

    private void initMap(Map<BinaryExpression.Operator, Instruction2> instructionASMD) {
        instructionASMD.put(BinaryExpression.Operator.ADD, (reg1, reg2, reg3) -> output.emitInstruction("add", new Register(reg1), new Register(reg2), new Register(reg3)));
        instructionASMD.put(BinaryExpression.Operator.SUB, (reg1, reg2, reg3) -> output.emitInstruction("sub", new Register(reg1), new Register(reg2), new Register(reg3)));
        instructionASMD.put(BinaryExpression.Operator.MUL, (reg1, reg2, reg3) -> output.emitInstruction("mul", new Register(reg1), new Register(reg2), new Register(reg3)));
        instructionASMD.put(BinaryExpression.Operator.DIV, (reg1, reg2, reg3) -> output.emitInstruction("div", new Register(reg1), new Register(reg2), new Register(reg3)));
    }


    public void generateCode(CallStatement callStatement, SymbolTable table) {
        ProcedureEntry procedureEntry = (ProcedureEntry) table.lookup(callStatement.procedureName);

        for (int i = 0; i < callStatement.arguments.size(); i++) {
            var arg = callStatement.arguments.get(i);
            var offset = procedureEntry.parameterTypes.get(i).offset;

            if (procedureEntry.parameterTypes.get(i).isReference) {
                generateCode(((VariableExpression) arg).variable, table);
            } else {
                generateCode(arg, table);
            }

            output.emitInstruction("stw", new Register(registerCounter - 1), Register.STACK_POINTER, offset, "store argument #" +i);
            registerCounter--;
        }
        output.emitInstruction("jal", callStatement.procedureName.toString());
        registerCounter = 8;

    }
    public void generateCode(Expression expression, SymbolTable table) {
        switch (expression){
            case VariableExpression variableExpression -> {
                generateCode(variableExpression.variable, table);
                output.emitInstruction("ldw", new Register(registerCounter-1), new Register(registerCounter-1), 0);
            }
            case IntLiteral intLiteral -> {
                output.emitInstruction("add", new Register(registerCounter), Register.NULL, intLiteral.value);
                if (isAvailableRegister()){
                    registerCounter++;
                }
            }
            case BinaryExpression binaryExpression -> {
                generateCode(binaryExpression.leftOperand,table);
                generateCode(binaryExpression.rightOperand,table);
                if (instructionASMD.get(binaryExpression.operator) != null){
//                    System.out.println(binaryExpression.operator.operatorString());
                    instructionASMD.get(binaryExpression.operator).execute(registerCounter - 2, registerCounter - 2, registerCounter -1);
                }


                registerCounter--;
            }
            case UnaryExpression unaryExpression -> {
                generateCode(unaryExpression.operand,table);
                output.emitInstruction("sub", new Register(registerCounter-1), new Register (0), new Register(registerCounter-1));
            }
        }
    }


    public void generateCode(AssignStatement assignStatement, SymbolTable table) {
        generateCode(assignStatement.target, table);
        generateCode(assignStatement.value, table);
        output.emitInstruction("stw", new Register(registerCounter-1), new Register(registerCounter-2), 0);
        registerCounter -= 2;
    }
    public void generateCode(Variable variable, SymbolTable table) {
        switch (variable){
            case NamedVariable namedVariable ->{
                generateCode(namedVariable,  table);
            }
            case ArrayAccess arrayAccess ->{
                generateCode(arrayAccess, table);
            }
        }

    }
    public void generateCode(CompoundStatement compoundStatement, SymbolTable table) {
        compoundStatement.statements.forEach( statement -> {
            generateCode(statement, table)  ;
        });
    }
    public void generateCode(NamedVariable variable, SymbolTable table) {
        VariableEntry entry = (VariableEntry) table.lookup(variable.name);
        output.emitInstruction("add", new Register(registerCounter), Register.FRAME_POINTER, entry.offset);
        if (entry.isReference) {
            output.emitInstruction("ldw", new Register(registerCounter), new Register(registerCounter), 0);
        }
        if (isAvailableRegister()){
            registerCounter++;
        }

    }

    public void generateCode(ArrayAccess arrayAccess, SymbolTable table) {
        var baseType = ((ArrayType) arrayAccess.array.type).baseType;
        var sizeOfArray = ((ArrayType) arrayAccess.array.type).arraySize;
        generateCode(arrayAccess.array, table);
        generateCode(arrayAccess.index, table);
        output.emitInstruction("add", new Register(registerCounter), new Register(0), sizeOfArray);
        if(isAvailableRegister()){
            countOfLabels++;
        }
        output.emitInstruction("bgeu", new Register(registerCounter-1), new Register(registerCounter), "_indexError");
        output.emitInstruction("mul", new Register(registerCounter - 1), new Register(registerCounter - 1), baseType.byteSize);
        countOfLabels--;
        output.emitInstruction("add", new Register(registerCounter - 2), new Register(registerCounter - 2), new Register(registerCounter - 1));
        registerCounter--;
    }

    public void generateCode(IfStatement ifStatement ,SymbolTable table) {
        String elseLabel = inkrementAntReturnLabel();
        String endLabel = inkrementAntReturnLabel();
        generateCode(ifStatement.condition, table);
        if (isAvailableRegister()){
            registerCounter++;
        }

        BinaryExpression.Operator operator = ((BinaryExpression)ifStatement.condition).operator;
        instructionsCompare.get(operator).execute(registerCounter - 2, registerCounter - 1, elseLabel);
        registerCounter -= 2;
        generateCode(ifStatement.thenPart, table);
        output.emitInstruction("j", endLabel);
        registerCounter = 8;
        output.emitLabel(elseLabel);
        generateCode(ifStatement.elsePart, table);
        output.emitLabel(endLabel);
    }
    public void generateCode(WhileStatement whileStatement, SymbolTable table) {
        String label1 = inkrementAntReturnLabel();
        String label2 = inkrementAntReturnLabel();

        output.emitLabel(label1);
        generateCode(whileStatement.condition, table);

        if (isAvailableRegister()) {
           registerCounter++;
        }
        BinaryExpression.Operator operator = ((BinaryExpression)whileStatement.condition).operator;
        instructionsCompare.get(operator).execute(registerCounter - 2, registerCounter - 1, label2);
        registerCounter -= 2;
        generateCode(whileStatement.body, table);
        output.emitInstruction("j", label1);
        output.emitLabel(label2);
    }


    private boolean isAvailableRegister() {
        if (24 > registerCounter){
            return true;
        }else{
            throw SplError.RegisterOverflow();
        }
    }

    private String inkrementAntReturnLabel(){
        return "L" + countOfLabels++;
    }

//
//    private void emitBinaryExpressionCode(BinaryExpression expression, SymbolTable symbolTable) {
//        emitExpressionCode(expression.leftOperand, symbolTable);
//        emitExpressionCode(expression.rightOperand, symbolTable);
//        String opCode = switch (expression.operator) {
//            case ADD -> "add";
//            case SUB -> "sub";
//            case MUL -> "mul";
//            case DIV -> "div";
//        };
//        printer.emitInstruction(opCode, new Register(regCounter - 2), new Register(regCounter - 2), new Register(regCounter - 1));
//        regCounter--;
//    }


    /**
     * Emits needed import statements, to allow usage of the predefined functions and sets the correct settings
     * for the assembler.
     */
    private void assemblerProlog() {
        output.emitImport("printi");
        output.emitImport("printc");
        output.emitImport("readi");
        output.emitImport("readc");
        output.emitImport("exit");
        output.emitImport("time");
        output.emitImport("clearAll");
        output.emitImport("setPixel");
        output.emitImport("drawLine");
        output.emitImport("drawCircle");
        output.emitImport("_indexError");
        output.emit("");
        output.emit("\t.code");
        output.emit("\t.align\t4");
    }
}
