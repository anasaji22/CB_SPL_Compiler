package de.thm.mni.compilerbau.absyn.visitor;

import de.thm.mni.compilerbau.CommandLineOptions;
import de.thm.mni.compilerbau.absyn.*;
import de.thm.mni.compilerbau.table.*;
import de.thm.mni.compilerbau.types.ArrayType;
import de.thm.mni.compilerbau.types.PrimitiveType;
import de.thm.mni.compilerbau.types.Type;
import de.thm.mni.compilerbau.utils.SplError;
import java.util.ArrayList;

public class ProcBodyCheckVisitor extends DoNothingVisitor {
    private SymbolTable globalTable;
    private SymbolTable lokaleTable;
    private ArrayList<ParameterType> parameterTypeList;
    private CommandLineOptions options;

    public ProcBodyCheckVisitor(SymbolTable globalTable, CommandLineOptions options) {
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
    }

//    //    type a = new Array of -..
////    type x = a
//    @Override
//    public void visit(TypeDefinition typeDefinition) {
//
//    }

    public void visit(ProcedureDefinition procedureDefinition) {
        var entry = (ProcedureEntry) globalTable.lookup(procedureDefinition.name);
        lokaleTable = entry.localTable;
        procedureDefinition.body.stream().forEach(statement -> {
            statement.accept(this);
        });
//        procedureDefinition.parameters.forEach(parameterDefinition -> {
//            parameterDefinition.accept(this);
//            if (!parameterDefinition.isReference && getType(lokaleTable, parameterDefinition.typeExpression) instanceof ArrayType ) {
//                throw SplError.ParameterMustBeReference(parameterDefinition.position, parameterDefinition.name, getType(lokaleTable, parameterDefinition.typeExpression));
//            }
//        });
    }

    @Override
    public void visit(AssignStatement assignStatement) {
        Type typeOfVar  = getType(lokaleTable, assignStatement.target);
        Type typeOfExpr =  getType(lokaleTable, assignStatement.value);
        if (typeOfExpr != typeOfVar){
            throw SplError.IllegalAssignment(assignStatement.position, typeOfVar, typeOfExpr);
        }
    }
    @Override
    public void visit(IfStatement  ifStatement) {
        var typeOfExpr = getType(lokaleTable, ifStatement.condition);
        if (typeOfExpr != PrimitiveType.boolType){
            throw  SplError.IfConditionMustBeBoolean(ifStatement.position, typeOfExpr);
        }
        ifStatement.thenPart.accept(this);
        ifStatement.elsePart.accept(this);
    }

    @Override
    public void visit( WhileStatement whileStatement) {
        var typeOfExpr = getType(lokaleTable, whileStatement.condition);
        if (typeOfExpr != PrimitiveType.boolType){
            throw SplError.WhileConditionMustBeBoolean(whileStatement.position, typeOfExpr);
        }
        whileStatement.body.accept(this);
    }
    @Override
    public void visit( CompoundStatement compoundStatement) {
        compoundStatement.statements.forEach( statements -> statements.accept(this) );
    }
    @Override
    public void visit( CallStatement callStatement) {
        var entry = globalTable.lookup(callStatement.procedureName, SplError.UndefinedIdentifier(callStatement.position, callStatement.procedureName));
        if (entry instanceof ProcedureEntry procedureEntry){

            for (int i = 0; i< procedureEntry.parameterTypes.size() ; i++){
//                System.out.println(callStatement.procedureName + "----------------" + callStatement.position.line);
//                System.out.println( procedureEntry.parameterTypes.size() );
//                System.out.println(procedureEntry.parameterTypes.get(i) );
//                System.out.println("---------Call Arg---------- \n" + callStatement.arguments.get(i));
//                System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><");

                var expectedType = procedureEntry.parameterTypes.get(i) ;

                var parameter = callStatement.arguments.get(i);
                var parameterType = getType(lokaleTable, parameter);
                if (parameterType != expectedType.type){
                    throw SplError.ArgumentTypeMismatch(callStatement.position, callStatement.procedureName, i+1,expectedType.type,parameterType);
                }
                if (expectedType.isReference && !(parameter instanceof VariableExpression)){
                    throw  SplError.ArgumentMustBeAVariable(callStatement.position, callStatement.procedureName,i+1);
                }
            }
            //TODO HANDLE HIER EROOR
//            System.out.println(">>>>>>>>>>>>>>>>>>>>> \n " + globalTable +"\n" + ">>>>>>>>>>>>>>>>>><< \n" );
//            System.out.println("ARgu>>>>>>>>>>>>   " + callStatement.arguments.size() );
//            System.out.println("------------------" + callStatement.arguments);
//            System.out.println("------------------------------------------------------------");
//            System.out.println("proc param Siz>>>>>>>>>>>>   " + procedureEntry.parameterTypes.size() );
//            System.out.println("proc param Siz>>>>>>>>>>>>   " + procedureEntry.parameterTypes);
            if (callStatement.arguments.size() != procedureEntry.parameterTypes.size() ){
                throw SplError.ArgumentCountMismatch(callStatement.position, callStatement.procedureName,procedureEntry.parameterTypes.size() ,callStatement.arguments.size() );
            }
        } else {
                throw SplError.CallOfNonProcedure(callStatement.position, callStatement.procedureName);
        }
    }

    @Override
    public void visit(NamedVariable namedVariable) {
        var entry = globalTable.lookup(namedVariable.name, SplError.UndefinedIdentifier(namedVariable.position, namedVariable.name));
        if (!(entry instanceof VariableEntry variableEntry)) {
            throw SplError.NotAVariable(namedVariable.position, namedVariable.name);
        }
    }


    @Override
    public void visit(ArrayAccess arrayAccess) {

    }

    public Type getType(SymbolTable table, Expression expression) {
        switch (expression) {
            case IntLiteral intLiteral -> {
                return PrimitiveType.intType;
            }
            case BinaryExpression binaryExpression -> {
                var leftType = getType(table, binaryExpression.leftOperand);
                var rightType = getType(table, binaryExpression.rightOperand);
                if (leftType != rightType) {
                    throw SplError.OperandTypeMismatch(expression.position, binaryExpression.operator, leftType, rightType);
                }
                if (binaryExpression.operator.isArithmetic()) {
                    return PrimitiveType.intType;
                } else if (binaryExpression.operator.isComparison()) {
                    return PrimitiveType.boolType;

                } else {
                    return null;
                }
            }
            case UnaryExpression unaryExpression -> {
                var rightType = getType(table, unaryExpression.operand);
                if (rightType != PrimitiveType.intType) {
                    throw SplError.OperandTypeMismatch(expression.position, unaryExpression.operator, rightType);
                } else {
                    return PrimitiveType.intType;
                }

            }
            case VariableExpression variableExpression -> {
                return getType(table, variableExpression.variable);//TODO change Null
            }
        }
    }

    public Type getType(SymbolTable table, Variable variable) {
        switch (variable) {
            case NamedVariable var: {
                var entry = table.lookup(var.name, SplError.UndefinedIdentifier(var.position, var.name));

                if (entry instanceof VariableEntry variableEntry) {
                    variable.type = variableEntry.type;
                    return variableEntry.type;
                } else {
                    throw SplError.NotAVariable(var.position, var.name);
                }
            }
            case ArrayAccess arrayAccess: {
                var arrayEntry = getType(table, arrayAccess.array);
                var arrayExp = getType(table, arrayAccess.index);
                if (arrayEntry instanceof ArrayType arrayType && arrayExp == PrimitiveType.intType) {
                    variable.type = ((ArrayType) arrayEntry).baseType;
                    return arrayType.baseType ;
                } else if (!(arrayEntry instanceof ArrayType arrayType)) {
                    throw SplError.IndexingNonArray(arrayAccess.position, arrayEntry);
                } else if (!(arrayExp == PrimitiveType.intType)) {
                    throw SplError.IndexTypeMismatch(arrayAccess.position, arrayEntry);
                } else {
                    return null;
                }
            }

        }
    }
//    public Type getType(SymbolTable table, TypeExpression typeExpression) {
//        switch (typeExpression) {
//            case NamedTypeExpression namedTypeExpression -> {
//                var entry = table.lookup(namedTypeExpression.name, SplError.UndefinedIdentifier(namedTypeExpression.position, namedTypeExpression.name));
//                if (entry instanceof TypeEntry typeEntry) {
//                    return typeEntry.type;
//                } else {
//                    throw SplError.NotAType(namedTypeExpression.position, namedTypeExpression.name);
//                }
//            }
//            case ArrayTypeExpression arrayTypeExpression -> {
//                return new ArrayType(getType(table, arrayTypeExpression.baseType), arrayTypeExpression.arraySize);
//            }
//        }
//    }

}
