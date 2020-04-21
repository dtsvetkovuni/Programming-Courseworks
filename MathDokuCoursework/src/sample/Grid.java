package sample;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;
import java.util.Stack;

public class Grid {
    private int size;
    public Cell currentCell;
    public Cell[][] gridCells;
    private int row;
    private int column;
    public ArrayList<Cage> cagelist;
    public Cell[][] randGridSolution;
    public Stack<Cell> stackUndo;
    public Stack<Cell> stackRedo;

    // Grid has size and sets the currentCell to 0 0
    public Grid(int size){
        this.size=size;
        gridCells = new Cell[size][size];
        cagelist = new ArrayList<>();
        randGridSolution = new Cell[size][size];
        stackUndo = new Stack<>();
        stackRedo = new Stack<>();
        this.makeGrid();
        currentCell = gridCells[0][0];
    }

    public void setSize(int size) {
        this.size = size;
    }

    public int getSize() {
        return size;
    }

    // makes a simple gridCells SizexSize filled with 0s
    public void makeGrid(){
        for(row=0;row<size;row++){
            for(column=0;column<size;column++)
                gridCells[row][column]=new Cell(0,row,column);
        }
    }


    // Checks if values in are win condition
    public boolean checkWin(){
        for (Cage cage: this.cagelist) {
            if(!cage.checkSolution()) {
                return false;
            }
        }

        ArrayList<Integer> cellValuesRow = new ArrayList<>();
        ArrayList<Integer> cellValuesColumn = new ArrayList<>();

        // row and column duplicate check
        for(int i=0;i<this.size;i++){
            for (int j=0;j<this.size;j++){
                cellValuesRow.add(this.gridCells[i][j].getValue());
                cellValuesColumn.add(this.gridCells[j][i].getValue());
            }
            for(int k=1;k<=this.size;k++){
                if(cellValuesRow.indexOf(k) != cellValuesRow.lastIndexOf(k)) return false;
                if(cellValuesColumn.indexOf(k) != cellValuesColumn.lastIndexOf(k)) return false;
            }
            cellValuesRow.clear();
            cellValuesColumn.clear();
        }
        return true;
    }


    /**UNDO REDO FUNCTIONALITY**/
    public void undoAdd(Cell cell){
        stackUndo.push(new Cell(cell.getValue(),cell.getRow(),cell.getColumn()));
        stackRedo.clear();
    }

    public void undoButtonClicked(){
        if(canUndo()){
            stackRedo.push(new Cell(stackUndo.peek().getValue(),stackUndo.peek().getRow(),stackUndo.peek().getColumn()));
            currentCell = stackUndo.pop();
        }
    }

    public void redoButtonClicked(){
        if(canRedo()) {
            stackUndo.push(new Cell(stackRedo.peek().getValue(), stackRedo.peek().getRow(), stackRedo.peek().getColumn()));
            currentCell = stackRedo.pop();
        }
    }

    public boolean canUndo(){
        if(!stackUndo.empty()) return true;
        else return false;
    }

    public boolean canRedo(){
        if(!stackRedo.empty()) return true;
        else return false;
    }

    public void clearGrid(){
        for(row=0;row<size;row++){
            for(column=0;column<size;column++)
                gridCells[row][column].setValue(0);
        }
        currentCell = gridCells[0][0];
        stackUndo = new Stack<>();
        stackRedo = new Stack<>();
    }

    public void clearCell(){
        currentCell.setValue(0);
    }

    /**UNDO REDO FUNCTIONALITY**/

    public Cell getCell(int row,int column){
        return gridCells[row][column];
    }

    public Cell getCurrentCell() {
        return currentCell;
    }

    public void setCurrentCell(Cell currentCell) {
        this.currentCell = currentCell;
    }
}
