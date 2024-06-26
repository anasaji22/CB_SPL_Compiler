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
    }

    @Override
    public void visit(AssignStatement assignStatement) {
        Type typeOfVar  = getType(lokaleTable, assignStatement.target);
        Type typeOfExpr =  getType(lokaleTable, assignStatement.value);
        if (typeOfExpr != typeOfExpr){
            throw SplError.IllegalAssignment(assignStatement.position, typeOfVar, typeOfExpr);
        }
    }
    @Override
    public void visit(IfStatement  ifStatement) {
        var typeOfExpr = getType(lokaleTable, ifStatement.condition);
        if (typeOfExpr != PrimitiveType.boolType){
            throw  SplError.IfConditionMustBeBoolean(ifStatement.position, typeOfExpr);
        }

        //TODO HANDLE ELSE
    }

    @Override
    public void visit( WhileStatement whileStatement) {
        var typeOfExpr = getType(lokaleTable, whileStatement.condition);
        if (typeOfExpr != PrimitiveType.boolType){
            throw SplError.WhileConditionMustBeBoolean(whileStatement.position, typeOfExpr);
        }

        //TODO HANDEL WHILE in WHILE
    }
    @Override
    public void visit( CompoundStatement compoundStatement) {
        compoundStatement.statements.forEach( statements -> statements.accept(this) );
    }
    @Override
    public void visit( CallStatement callStatement) {
        var entry = lokaleTable.lookup(callStatement.procedureName, SplError.UndefinedIdentifier(callStatement.position, callStatement.procedureName));
        if (entry instanceof ProcedureEntry procedureEntry){
            for (int i = 0; i< procedureEntry.parameterTypes.size() -1; i++){
                var varParamTyp = procedureEntry.parameterTypes.get(i);
                var argument = callStatement.arguments.get(i);
                var argumentTyp = getType(globalTable, argument);
                if (argumentTyp != varParamTyp.type){
                    throw SplError.ArgumentTypeMismatch(callStatement.position, callStatement.procedureName, i+1,varParamTyp.type,argumentTyp);
                }
                if (varParamTyp.isReference && !(argument instanceof VariableExpression)){
                    throw  SplError.ArgumentMustBeAVariable(callStatement.position, callStatement.procedureName,i+1);
                }
            }
            //TODO HANDLE HIER EROOR
            System.out.println("ARgu>>>>>>>>>>>>   " + callStatement.arguments.size() );
            System.out.println("------------------" + callStatement.arguments);
            System.out.println("------------------------------------------------------------");
            System.out.println("proc param Siz>>>>>>>>>>>>   " + procedureEntry.parameterTypes.size() );
            System.out.println("proc param Siz>>>>>>>>>>>>   " + procedureEntry.parameterTypes);
            if (callStatement.arguments.size() != procedureEntry.parameterTypes.size() ){
                throw SplError.ArgumentCountMismatch(callStatement.position, callStatement.procedureName,procedureEntry.parameterTypes.size() -1  ,callStatement.arguments.size() );
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
                    return variableEntry.type;
                } else {
                    throw SplError.NotAVariable(var.position, var.name);
                }
            }
            case ArrayAccess arrayAccess: {
                var arrayEntry = getType(table, arrayAccess.array);
                var arrayExp = getType(table, arrayAccess.index);
                if (arrayEntry instanceof ArrayType arrayType && arrayExp == PrimitiveType.intType) {
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


}
