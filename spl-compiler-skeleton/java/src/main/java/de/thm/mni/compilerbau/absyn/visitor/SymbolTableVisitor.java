package de.thm.mni.compilerbau.absyn.visitor;

import de.thm.mni.compilerbau.CommandLineOptions;
import de.thm.mni.compilerbau.absyn.*;
import de.thm.mni.compilerbau.table.SymbolTable;
import de.thm.mni.compilerbau.table.*;
import de.thm.mni.compilerbau.types.*;
import de.thm.mni.compilerbau.types.ArrayType;
import de.thm.mni.compilerbau.utils.SplError;
import de.thm.mni.compilerbau.phases._04a_tablebuild.TableBuilder.*;
import java.util.ArrayList;

import static de.thm.mni.compilerbau.phases._04a_tablebuild.TableBuilder.printSymbolTableAtEndOfProcedure;

public class SymbolTableVisitor extends DoNothingVisitor {

    private SymbolTable globalTable;
    private SymbolTable lokaleTable;
    private ArrayList<ParameterType> parameterTypeList;
    private CommandLineOptions options;

    public SymbolTableVisitor(SymbolTable globalTable, CommandLineOptions options ) {

        this.globalTable = globalTable;
        this.options = options;
    }

    @Override
    public void visit(Program program) {
        program.definitions.stream().forEach(dec -> {
            if (dec instanceof TypeDefinition || dec instanceof ProcedureDefinition) {
                dec.accept(this);
            }
        });
        var main = globalTable.lookup(new Identifier("main"), SplError.MainIsMissing());
        if (!(main instanceof ProcedureEntry proEntry)){
            throw SplError.MainIsNotAProcedure();
        }
        if (!(proEntry.parameterTypes.isEmpty())){
            throw SplError.MainMustNotHaveParameters();
        }
    }

    @Override
    public void visit(TypeDefinition typeDefinition) {
        globalTable.enter(typeDefinition.name, new TypeEntry(getType(globalTable, typeDefinition.typeExpression)));
        if (typeDefinition.typeExpression instanceof NamedTypeExpression
                || typeDefinition.typeExpression instanceof ArrayTypeExpression) {
            typeDefinition.typeExpression.accept(this);
        }
    }

    @Override
    public void visit(ArrayTypeExpression arrayTypeExpression) {
        arrayTypeExpression.baseType.accept(this);

    }

    @Override
    public void visit(ProcedureDefinition procedureDefinition) {
        lokaleTable = new SymbolTable(globalTable);
        parameterTypeList = new ArrayList<>();
        procedureDefinition.variables.forEach(varDef -> {
            varDef.accept(this);
        });
        procedureDefinition.parameters.forEach(parameterDefinition -> {
            parameterDefinition.accept(this);
            if (!(parameterDefinition.isReference) && getType(lokaleTable, parameterDefinition.typeExpression) instanceof ArrayType ) {
                throw SplError.ParameterMustBeReference(parameterDefinition.position, parameterDefinition.name, getType(lokaleTable, parameterDefinition.typeExpression));
            }

//            TODO muss das hier sethen oder in der Bodychecker und warum funktioniert es nicht an dem Beispeil?

//            proc p(ref x: int) {
//            }
//            proc tt() {
//                var a: array[5] of int;
//                p(a);
//            }
//            proc p(ref x: matrix) {
//            }
//
//            proc tt() {
//                var a: int;
//                var b: int;
//                p(b);
//            }


        });
        ProcedureEntry procedureEntry = new ProcedureEntry(lokaleTable, parameterTypeList);
        globalTable.enter(
                procedureDefinition.name,
                procedureEntry,
                SplError.RedefinitionOfIdentifier(procedureDefinition.position, procedureDefinition.name)
        );
        if (options.phaseOption == CommandLineOptions.PhaseOption.TABLES){
            printSymbolTableAtEndOfProcedure(procedureDefinition.name, procedureEntry);
        }

    }

    @Override
    public void visit(VariableDefinition variableDefinition) {
        lokaleTable.enter(
                variableDefinition.name,
                new VariableEntry(getType(lokaleTable, variableDefinition.typeExpression), false),
                SplError.RedefinitionOfIdentifier(variableDefinition.position,variableDefinition.name)
        );

    }

    @Override
    public void visit(ParameterDefinition parameterDefinition) {
        parameterTypeList.add(
                new ParameterType(getType(lokaleTable, parameterDefinition),
                        parameterDefinition.isReference)
        );


    }

    public Type getType(SymbolTable table, TypeExpression typeExpression) {
        switch (typeExpression) {
            case NamedTypeExpression namedTypeExpression -> {
                var entry = table.lookup(namedTypeExpression.name, SplError.UndefinedIdentifier(namedTypeExpression.position, namedTypeExpression.name));
                if (entry instanceof TypeEntry typeEntry) {
                    return typeEntry.type;
                } else {
                    throw SplError.NotAType(namedTypeExpression.position, namedTypeExpression.name);
                }
            }
            case ArrayTypeExpression arrayTypeExpression -> {
                return new ArrayType(getType(table, arrayTypeExpression.baseType), arrayTypeExpression.arraySize);
            }
        }
    }
    public Type getType(SymbolTable table, ParameterDefinition parameterDefinition) {
      var parameterType = getType(table, parameterDefinition.typeExpression);
      VariableEntry variableEntry = new VariableEntry(parameterType,parameterDefinition.isReference);
        table.enter(parameterDefinition.name,
                    variableEntry,
                    SplError.RedefinitionOfIdentifier(parameterDefinition.position,parameterDefinition.name)
        );
        //TODO handel Eroor aber welchen ???
        return parameterType;
    }

    public SymbolTable getGlobalTable() {
        return globalTable;
    }

    public SymbolTable getLokaleTable() {
        return lokaleTable;
    }
}
