package sample;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

public class Cage {
    public ArrayList<Cell> cellsInCage;
    public int finalSolution;
    enum operations {
        ADDITION,
        SUBTRACTION,
        MULTIPLICATION,
        DIVISION
    }
    public operations operation;
    private Grid grid;

    public Cage(int finalSolution,operations operation,ArrayList<Integer> cellNumbers,int size){
        this.finalSolution=finalSolution;
        this.operation=operation;
        cellsInCage = new ArrayList<>();
        for (Integer i:cellNumbers) {
            cellsInCage.add(new Cell(0,(i-1)/size,(i-1)%size));
        }
    }

    // takes the grid
    public void setGrid(Grid grid) {
        this.grid = grid;
        this.setCellsInCage();
    }

    // takes cell from grid
    public void setCellsInCage() {
        for (int i=0;i<cellsInCage.size();i++) {
            cellsInCage.set(i,grid.getCell(cellsInCage.get(i).getRow(),cellsInCage.get(i).getColumn()));
        }
    }

    public boolean checkSolution(){
        int value=0;
        // checks all values
        if(operation.equals(operations.ADDITION)){
            value=0;
            for (Cell cell:cellsInCage) {
                if(cell.getValue()==0) return false;
                value = value + cell.getValue();
            }
        }

        if(operation.equals(operations.MULTIPLICATION)){
            value=1;
            for (Cell cell:cellsInCage) {
                if(cell.getValue()==0) return false;
                value = value*cell.getValue();
            }
        }

        // takes cell with maximum value
        Cell maximum= Collections.max(cellsInCage, Comparator.comparingInt(Cell::getValue));

        if(operation.equals(operations.SUBTRACTION)){
            value=maximum.getValue();
            int subFlag=0;
            for (Cell cell:cellsInCage) {
                if(cell.getValue()==0) return false;
                if(maximum.getValue()==cell.getValue() && subFlag==1) value = value- cell.getValue();
                if(maximum.getValue()!=cell.getValue()) value = value- cell.getValue();
                else subFlag=1;
            }
        }

        if(operation.equals(operations.DIVISION)){
            value=maximum.getValue();
            int divFlag=0;
            for (Cell cell:cellsInCage) {
                if(cell.getValue()==0) return false;
                if(maximum.getValue()==cell.getValue() && divFlag==1){
                    if(value%cell.getValue() == 0) value = value/cell.getValue();
                    else return false;
                }
                if(maximum.getValue()!=cell.getValue()){
                    if(value%cell.getValue() == 0) value = value/cell.getValue();
                    else return false;
                } else divFlag=1;
            }
        }

        if(value == finalSolution) {
            return true;
        } else {
            return false;
        }
    }

    public ArrayList<Cell> getCage() {
        return cellsInCage;
    }

    public Grid getGrid() {
        return grid;
    }

    public void setCage(ArrayList<Cell> cage) {
        this.cellsInCage = cage;
    }

    public void setOperation(operations operation) {
        this.operation = operation;
    }

    public int getFinalSolution() {
        return finalSolution;
    }

    public void setFinalSolution(int finalSolution) {
        this.finalSolution = finalSolution;
    }
}
