package de.thm.mni.compilerbau.absyn.visitor;

import de.thm.mni.compilerbau.absyn.*;
import de.thm.mni.compilerbau.table.ParameterType;
import de.thm.mni.compilerbau.table.ProcedureEntry;
import de.thm.mni.compilerbau.table.SymbolTable;
import de.thm.mni.compilerbau.table.VariableEntry;

import javax.swing.text.html.Option;
public class VarAllocatorVisitor extends DoNothingVisitor {
    private final Program program;
    private final SymbolTable symbolTable;
    private ProcedureEntry procedureEntry;
    private VariableEntry variableEntry;
    private ParameterType parameterType;
    private final int REFERENCE_BYTESIZE;
    private int index;
    private int parameterAdresse = 0;
    private int variableAdresse = 0;
    private int outgoingAreaSize = 0;

    public VarAllocatorVisitor(Program program, SymbolTable symbolTable, int REFERENCE_BYTESIZE)  {
        this.program = program;
        this.symbolTable = symbolTable;
        this.REFERENCE_BYTESIZE = REFERENCE_BYTESIZE;
    }

    @Override
    public void visit(ProcedureDefinition procedureDefinition) {
        // Lookup procedure entry
        procedureEntry = (ProcedureEntry) symbolTable.lookup(procedureDefinition.name);

        if (procedureEntry == null) {
            throw new IllegalArgumentException("ProcedureEntry not found for " + procedureDefinition.name);
        }

        index = 0;
        parameterAdresse = 0;
        // Process parameters
        for (ParameterDefinition parameterDefinition : procedureDefinition.parameters) {
            variableEntry = (VariableEntry) procedureEntry.localTable.lookup(parameterDefinition.name);
            parameterType = procedureEntry.parameterTypes.get(index);
            index++;
            parameterType.offset = parameterAdresse;
            variableEntry.offset = parameterAdresse;
            parameterAdresse += !variableEntry.isReference ? variableEntry.type.byteSize : REFERENCE_BYTESIZE;
//            System.out.println("_____OFSET parameterType.offsetm: " + parameterType.offset);
        }

//        System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
//        System.out.println("----" + procedureDefinition.name + "----");
        procedureEntry.stackLayout.argumentAreaSize = parameterAdresse;
//        System.out.println("--argumentAreaSize--" + parameterAdresse + "----");

        parameterAdresse = 0;
        variableAdresse = 0;
        // Process local variables
        for (VariableDefinition variableDefinition : procedureDefinition.variables) {
            variableEntry = (VariableEntry) procedureEntry.localTable.lookup(variableDefinition.name);
            variableAdresse += variableEntry.type.byteSize;
            variableEntry.offset = -variableAdresse;
//            System.out.println("_____OFSet Variable: " + variableEntry.offset);
        }
//        System.out.println(">>>>>>>>>>localVarAreaSize>>>>>>"+ variableAdresse);
        procedureEntry.stackLayout.localVarAreaSize = variableAdresse;
    }

    @Override
    public void visit(Program program) {
        // First pass: allocate parameters and local variables
        for (var def : program.definitions) {
            if (def instanceof ProcedureDefinition) {
                def.accept(this);
            }
        }

        // Second pass: calculate outgoing area sizes
        for (var def : program.definitions) {
            if (def instanceof ProcedureDefinition) {
                procedureEntry = (ProcedureEntry) symbolTable.lookup(((ProcedureDefinition) def).name);
                outgoingAreaSize = 0;
                for (Statement statement : ((ProcedureDefinition) def).body) {
                    outgoingAreaSize = Math.max(calculateoutgoingAreaSize(statement), outgoingAreaSize);
                }
//                System.out.println(">>>>>>>>>>outgoingAreaSize>>>>>>"+ outgoingAreaSize);
                procedureEntry.stackLayout.outgoingAreaSize = outgoingAreaSize;
//                System.out.println(">>>>>>>> procedureEntry.stackLayout.argumentAreaSize>>>>>>" +  procedureEntry.stackLayout.argumentAreaSize);
//                System.out.println(">>>>>>>> procedureEntry.stackLayout.localVarAreaSize>>>>>>" +  procedureEntry.stackLayout.localVarAreaSize);
//                System.out.println(">>>>>>>> procedureEntry.stackLayout.outgoingAreaSize>>>>>>" +  procedureEntry.stackLayout.outgoingAreaSize);


            }
        }
    }

    public int calculateoutgoingAreaSize(Statement stat) {
        if (stat instanceof CallStatement) {
            CallStatement callStat = (CallStatement) stat;
            ProcedureEntry procedureEntry1 = (ProcedureEntry) symbolTable.lookup(callStat.procedureName);
            return procedureEntry1.stackLayout.argumentAreaSize;
        } else if (stat instanceof IfStatement) {
            IfStatement ifStat = (IfStatement) stat;
            int areaSizeThen = calculateoutgoingAreaSize(ifStat.thenPart);
            int areaSizeElse = calculateoutgoingAreaSize(ifStat.elsePart);
            return Math.max(areaSizeThen, areaSizeElse);
        } else if (stat instanceof WhileStatement) {
            WhileStatement whileStat = (WhileStatement) stat;
            return calculateoutgoingAreaSize(whileStat.body);
        } else if (stat instanceof CompoundStatement) {
            CompoundStatement compoundStat = (CompoundStatement) stat;
            int tempSize = 0;
            for (Statement comStatements : compoundStat.statements) {
                tempSize = Math.max(calculateoutgoingAreaSize(comStatements), tempSize);
            }
            return tempSize;
        } else {
            return 0;
        }
    }
}

