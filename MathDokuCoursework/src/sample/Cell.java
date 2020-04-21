package sample;

public class Cell
{
    private int value;
    private int row;
    private int column;
    public Cage cage;
    public boolean headCell;


    // Cells have a value and a flag if have value, row and column
    public Cell(int value,int row, int column){
            this.value=value;
            this.row=row;
            this.column=column;
            headCell=false;
    }

    public int getValue() {
        return value;
    }

    public void setValue(int value) {
        this.value=value;
    }

    public void setColumn(int column) {
        this.column = column;
    }

    public int getColumn() {
        return column;
    }

    public void setRow(int row) {
        this.row = row;
    }

    public int getRow() {
        return row;
    }
}
