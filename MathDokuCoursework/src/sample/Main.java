package sample;

import javafx.animation.Animation;
import javafx.animation.KeyFrame;
import javafx.animation.PauseTransition;
import javafx.animation.Timeline;
import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.TextAlignment;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import javafx.util.Duration;
import javafx.util.StringConverter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.*;

public class Main extends Application {
    Stage mainstage;
    Font numbersFont = new Font("Arial", 26);
    Font cageFont = new Font("Arial", 19);
    String currentFont="Medium";
    Color backColor=Color.rgb(219,255,137,1);
    int hintCounter=0;
    @Override
    public void start(Stage primaryStage) throws Exception {
        this.mainstage=primaryStage;
        this.mainstage.setResizable(false);
        this.startMenu();
        hintCounter=0;
}


    public static void main(String[] args) {
        launch(args);
    }

    // creates the Main Menu
    public void startMenu(){
        hintCounter=0;
        /**Main menu scene**/
        Slider slider = new Slider(2,8,5);
        slider.setMajorTickUnit(1);
        slider.setSnapToTicks(true);
        slider.setShowTickMarks(true);
        slider.setMinorTickCount(0);
        slider.setShowTickLabels(true);
        slider.setMaxWidth(300);
        slider.setLabelFormatter(new StringConverter<Double>() {
            @Override
            public String toString(Double n) {
                if (n == 2) return "2x2";
                if (n == 3) return "3x3";
                if (n == 4) return "4x4";
                if (n == 5) return "5x5";
                if (n == 6) return "6x6";
                if (n == 7) return "7x7";
                if (n == 8) return "8x8";
                return "5x5";
            }

            @Override
            public Double fromString(String s) {
                switch (s) {
                    case "2x2":
                        return 2d;
                    case "3x3":
                        return 3d;
                    case "4x4":
                        return 4d;
                    case "5x5":
                        return 5d;
                    case "6x6":
                        return 6d;
                    case "7x7":
                        return 7d;
                    case "8x8":
                        return 8d;
                    default:
                        return 5d;
                }
            }
        });
        slider.setStyle("-fx-background-color: #ffffff; -fx-font-size: 2em;");
        slider.valueProperty().addListener((obs, oldValue, newValue) -> slider.setValue(Math.round(newValue.doubleValue())));

        VBox vbox7 = new VBox(15);
        VBox vbox360 = new VBox(-4);
        vbox7.setAlignment(Pos.CENTER);
        vbox360.setAlignment(Pos.CENTER);

        Scene mainScene = new Scene(vbox7, 650, 650,Color.color(0.3,0.25,0.2,0.5));
        vbox7.setBackground(new Background(new BackgroundFill(backColor, CornerRadii.EMPTY, Insets.EMPTY)));

        Button playButton = new Button("Play Game");
        Button fileLoadButton = new Button("Load From File");
        Label formatLabel = new Label("Format: 3x 5,7,8");
        Button textLoadButton = new Button("Load From Input");
        Button fontSettingsButton = new Button("Font Size");
        Button quitButton = new Button("Quit");

        playButton.setMaxWidth(200);
        fileLoadButton.setMaxWidth(200);
        textLoadButton.setMaxWidth(200);
        fontSettingsButton.setMaxWidth(200);
        quitButton.setMaxWidth(200);

        TextArea text = new TextArea();
        text.setMinWidth(150);
        text.setMaxWidth(300);
        text.setMinHeight(100);
        text.setMaxHeight(200);
        text.setWrapText(true);
        text.setFont(numbersFont);
        text.setPromptText("Enter Grid");

        /**Buttons Styling**/
        formatLabel.setStyle("-fx-font-size: 2em;");
        text.setStyle("-fx-border-color: #ff0000; -fx-border-width: 4px;");
        playButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 4px; -fx-background-color: #00ff00; -fx-font-size: 2em;");
        fileLoadButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 4px; -fx-background-color: #ffd480; -fx-font-size: 2em;");
        textLoadButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 4px; -fx-background-color: #ffd480; -fx-font-size: 2em;");
        fontSettingsButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 4px; -fx-background-color: #ffd480; -fx-font-size: 2em;");
        quitButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffb3b3; -fx-font-size: 2em;");


        vbox360.getChildren().addAll(formatLabel,text,textLoadButton);

        vbox7.getChildren().addAll(slider,playButton,fileLoadButton,vbox360,fontSettingsButton,quitButton);

        playButton.setOnAction(e -> {
            int gridSize= (int) slider.getValue();
            this.createGame(gridSize);
        });
        /**FILE READER TESTING**/
        fileLoadButton.setOnAction(new EventHandler<ActionEvent>() {

            @Override
            public void handle(ActionEvent event) {
                FileChooser fileChooser = new FileChooser();
                int gridFileSize=0;
                fileChooser.setTitle("Open File to Load");
                FileChooser.ExtensionFilter txtFilter = new FileChooser.ExtensionFilter("Text files", "*.txt");
                fileChooser.getExtensionFilters().add(txtFilter);
                File file = fileChooser.showOpenDialog(mainstage);

                if (file != null && file.exists() && file.canRead()) {
                    try {
                        BufferedReader bufferReader = new BufferedReader(new FileReader(file));
                        String[] line;
                        String[] cellsIDs;
                        ArrayList<Integer> cellsInCage= new ArrayList<>();
                        Cage currentCage=null;
                        String operation;
                        int cageAnswer=0;
                        while (bufferReader.ready()) {
                            line=bufferReader.readLine().split(" ");
                            cellsIDs = line[1].split(",");
                            for (String id: cellsIDs) {
                                if(gridFileSize<Integer.parseInt(id))gridFileSize=Integer.parseInt(id);
                            }
                        }
                        bufferReader.close();
                        gridFileSize= (int)Math.sqrt(gridFileSize);
                        Grid grid=new Grid(gridFileSize);


                        bufferReader = new BufferedReader(new FileReader(file));
                        while (bufferReader.ready()) {
                            currentCage=null;
                            cellsInCage= new ArrayList<>();
                            cellsIDs=null;
                            line=null;
                            operation=null;
                            line = bufferReader.readLine().split(" ");
                            operation = line[0].substring(line[0].length() - 1);
                            cellsIDs = line[1].split(",");
                            for (String id : cellsIDs) {
                                cellsInCage.add(Integer.parseInt(id));
                            }

                            if(operation.equals("+") || operation.equals("-") ||operation.equals("x") ||operation.equals("÷")) {
                                cageAnswer = Integer.parseInt(line[0].substring(0, line[0].length() - 1));
                            } else if(operation.equals("0")||operation.equals("1")||operation.equals("2")||operation.equals("3")||operation.equals("4")||operation.equals("5")||operation.equals("6")||operation.equals("7")||operation.equals("8")||operation.equals("9")){
                                cageAnswer = Integer.parseInt(line[0]);
                                currentCage=new Cage(cageAnswer, Cage.operations.ADDITION, cellsInCage, gridFileSize);
                                grid.cagelist.add(currentCage);
                                currentCage.setGrid(grid);
                            }

                            if (operation.equals("+")) {
                                currentCage=new Cage(cageAnswer, Cage.operations.ADDITION, cellsInCage, gridFileSize);
                                grid.cagelist.add(currentCage);
                                currentCage.setGrid(grid);
                            } else if (operation.equals("-")) {
                                currentCage=new Cage(cageAnswer, Cage.operations.SUBTRACTION, cellsInCage, gridFileSize);
                                grid.cagelist.add(currentCage);
                                currentCage.setGrid(grid);
                            }
                            if (operation.equals("÷")) {
                                currentCage=new Cage(cageAnswer, Cage.operations.DIVISION, cellsInCage, gridFileSize);
                                grid.cagelist.add(currentCage);
                                currentCage.setGrid(grid);
                            }
                            if (operation.equals("x")) {
                                currentCage=new Cage(cageAnswer, Cage.operations.MULTIPLICATION, cellsInCage, gridFileSize);
                                grid.cagelist.add(currentCage);
                                currentCage.setGrid(grid);
                            }
                            for (Cell cell:currentCage.cellsInCage) {
                                cell.cage=currentCage;
                            }
                            currentCage.cellsInCage.get(0).headCell=true;
                        }
                        bufferReader.close();
                        createGame(gridFileSize,grid);
                    } catch (Exception e) {
                        Alert alert = new Alert(Alert.AlertType.WARNING);
                        alert.setTitle("Oops!");
                        alert.setHeaderText("Wrong File Format!\nCheck your file input!");
                        alert.setContentText("Press OK to try again.");
                        Optional<ButtonType> result = alert.showAndWait();
                        if (result.isPresent() && result.get() == ButtonType.OK) {}
                    }
                }
            }

        });
        textLoadButton.setOnAction(new EventHandler<ActionEvent>() {

            @Override
            public void handle(ActionEvent event) {
                try {
                    int gridFileSize = 0;
                    String[] line;
                    String[] cellsIDs;
                    ArrayList<Integer> cellsInCage = new ArrayList<>();
                    Cage currentCage = null;
                    String operation;
                    int cageAnswer = 0;
                    for (String singleLine : text.getText().split("\\n")) {
                        line=singleLine.split(" ");
                        cellsIDs = line[1].split(",");
                        for (String id: cellsIDs) {
                            if(gridFileSize<Integer.parseInt(id))gridFileSize=Integer.parseInt(id);
                        }
                    }

                    gridFileSize= (int)Math.sqrt(gridFileSize);
                    Grid grid=new Grid(gridFileSize);

                    for (String singleLine : text.getText().split("\\n")) {
                        currentCage=null;
                        cellsInCage= new ArrayList<>();
                        cellsIDs=null;
                        line=null;
                        operation=null;
                        line = singleLine.split(" ");
                        operation = line[0].substring(line[0].length() - 1);
                        cellsIDs = line[1].split(",");

                        for (String id : cellsIDs) {
                            cellsInCage.add(Integer.parseInt(id));
                        }
                        if(operation.equals("+") || operation.equals("-") ||operation.equals("x") ||operation.equals("÷")) {
                            cageAnswer = Integer.parseInt(line[0].substring(0, line[0].length() - 1));
                        } else if(operation.equals("0")||operation.equals("1")||operation.equals("2")||operation.equals("3")||operation.equals("4")||operation.equals("5")||operation.equals("6")||operation.equals("7")||operation.equals("8")||operation.equals("9")){
                            cageAnswer = Integer.parseInt(line[0]);
                            currentCage=new Cage(cageAnswer, Cage.operations.ADDITION, cellsInCage, gridFileSize);
                            grid.cagelist.add(currentCage);
                            currentCage.setGrid(grid);
                        }

                        if (operation.equals("+")) {
                            currentCage=new Cage(cageAnswer, Cage.operations.ADDITION, cellsInCage, gridFileSize);
                            grid.cagelist.add(currentCage);
                            currentCage.setGrid(grid);
                        } else if (operation.equals("-")) {
                            currentCage=new Cage(cageAnswer, Cage.operations.SUBTRACTION, cellsInCage, gridFileSize);
                            grid.cagelist.add(currentCage);
                            currentCage.setGrid(grid);
                        }
                        if (operation.equals("÷")) {
                            currentCage=new Cage(cageAnswer, Cage.operations.DIVISION, cellsInCage, gridFileSize);
                            grid.cagelist.add(currentCage);
                            currentCage.setGrid(grid);
                        }
                        if (operation.equals("x")) {
                            currentCage=new Cage(cageAnswer, Cage.operations.MULTIPLICATION, cellsInCage, gridFileSize);
                            grid.cagelist.add(currentCage);
                            currentCage.setGrid(grid);
                        }
                        for (Cell cell:currentCage.cellsInCage) {
                            cell.cage=currentCage;
                        }
                        currentCage.cellsInCage.get(0).headCell=true;
                    }
                    createGame(gridFileSize,grid);

                }catch (Exception e) {
                    Alert alert = new Alert(Alert.AlertType.WARNING);
                    alert.setTitle("Oops!");
                    alert.setHeaderText("There has been a mistake within your input!\nCheck your input!");
                    alert.setContentText("Press OK to try again.");
                    Optional<ButtonType> result = alert.showAndWait();
                    if (result.isPresent() && result.get() == ButtonType.OK) {}
                }
            }

        });
        /**FILE READER TESTING END**/

        fontSettingsButton.setOnAction(e -> {
            List<String> choices = new ArrayList<>();
            choices.add("Small");
            choices.add("Medium");
            choices.add("Large");
            choices.add("Large+");
            ChoiceDialog<String> dialog = new ChoiceDialog<String>(currentFont, choices);
            dialog.setTitle("Font Size");
            dialog.setHeaderText("Select desired font size.");
            dialog.setContentText(null);
            Optional<String> resultFont = dialog.showAndWait();
            if (resultFont.isPresent()) {
                if(resultFont.get().equals("Small")){
                    numbersFont = new Font("Arial", 23);
                    cageFont = new Font("Arial", 15);
                } else if(resultFont.get().equals("Medium")){
                    numbersFont = new Font("Arial", 26);
                    cageFont = new Font("Arial", 19);
                }else if(resultFont.get().equals("Large")){
                    numbersFont = new Font("Arial", 29);
                    cageFont = new Font("Arial", 21);
                }else if(resultFont.get().equals("Large+")){
                    numbersFont = new Font("Arial", 32);
                    cageFont = new Font("Arial", 23);
                }
                currentFont=resultFont.get();
            }
        });

        quitButton.setOnAction(e -> {
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
            alert.setTitle("Quit?");
            alert.setHeaderText("Do you really want to quit MathDoku");
            alert.setContentText(null);
            Optional<ButtonType> result = alert.showAndWait();
            if (result.isPresent() && result.get() == ButtonType.OK) {mainstage.close();}
        });

        mainstage.setTitle("MathDoku");
        mainstage.setScene(mainScene);
        mainstage.show();
    }

    // creates the random grid menu
    public void createGame(int gridSize){
        int width=225+100*gridSize;
        int height=100+100*gridSize;

        Grid myGrid = new Grid(gridSize);
        GridPane gridP = new GridPane();
        GridPane gridButtons = new GridPane();
        gridButtons.setHgap(5);
        gridButtons.setVgap(5);

        Pane insidePane = new Pane();
        BorderPane borderP = new BorderPane();
        borderP.setBackground(new Background(new BackgroundFill(backColor, CornerRadii.EMPTY, Insets.EMPTY)));

        borderP.setPadding(new Insets(10));
        HBox buttonBox = new HBox(5);
        buttonBox.setAlignment(Pos.CENTER);
        if(gridSize==2){
            width=415;
            height=442;
            buttonBox.setAlignment(Pos.TOP_LEFT);
        }
        if(gridSize==3){
            width=525;
            height=442;
            buttonBox.setAlignment(Pos.TOP_LEFT);
        }

        Scene gameScene = new Scene(borderP, width, height);

        /**Generates random Grid**/
        makeRandomGame(myGrid, gridSize);

        Button undoButton = new Button("Undo");
        Button redoButton = new Button("Redo");
        undoButton.setDisable(true);
        redoButton.setDisable(true);

        Button clearButton = new Button("Clear Board");
        Button showHintButton = new Button("Hint");
        Button showSolutionButton = new Button("Show Solution");
        CheckBox showMistakesCheckbox = new CheckBox("Show Mistakes");
        showMistakesCheckbox.setPadding(new Insets(3));
        Button oneButton = new Button("1");
        Button twoButton = new Button("2");
        Button threeButton = new Button("3");
        Button fourButton = new Button("4");
        Button fiveButton = new Button("5");
        Button sixButton = new Button("6");
        Button sevenButton = new Button("7");
        Button eightButton = new Button("8");
        Button deleteButton = new Button("Delete");
        Button mainMenuButton = new Button("Main Menu");
        Button quitGameButton = new Button("Quit Game");

        /**Button styles**/
        oneButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        twoButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        threeButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        fourButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        fiveButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        sixButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        sevenButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        eightButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        deleteButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffb3b3; -fx-font-size: 20px;");


        undoButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        redoButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        clearButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        showHintButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        showSolutionButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        showHintButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        mainMenuButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        quitGameButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        showMistakesCheckbox.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        undoButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffd480; -fx-font-size: 20px;");
        redoButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffd480; -fx-font-size: 20px;");
        clearButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #f2f2f2; -fx-font-size: 20px;");
        showHintButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #99ff99; -fx-font-size: 20px;");
        showMistakesCheckbox.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffd480; -fx-font-size: 20px;");
        showSolutionButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #66ff66; -fx-font-size: 20px;");
        mainMenuButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffd480; -fx-font-size: 20px;");
        quitGameButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffb3b3; -fx-font-size: 20px;");


        insidePane.getChildren().add(gridP);
        borderP.setCenter(insidePane);
        borderP.setRight(gridButtons);
        borderP.setBottom(buttonBox);

        gridButtons.add(undoButton, 0, 0, 1, 1);
        gridButtons.add(redoButton, 1, 0, 1, 1);
        gridButtons.add(clearButton, 0, 1, 2, 1);
        gridButtons.add(showHintButton, 0, 3, 2, 1);
        gridButtons.add(showSolutionButton, 0, 4, 2, 1);
        gridButtons.add(showMistakesCheckbox, 0, 5, 2, 1);
        if (gridSize >= 1) buttonBox.getChildren().add(oneButton);
        if (gridSize >= 2) buttonBox.getChildren().add(twoButton);
        if (gridSize >= 3) buttonBox.getChildren().add(threeButton);
        if (gridSize >= 4) buttonBox.getChildren().add(fourButton);
        if (gridSize >= 5) buttonBox.getChildren().add(fiveButton);
        if (gridSize >= 6) buttonBox.getChildren().add(sixButton);
        if (gridSize >= 7) buttonBox.getChildren().add(sevenButton);
        if (gridSize >= 8) buttonBox.getChildren().add(eightButton);
        buttonBox.getChildren().add(deleteButton);
        gridButtons.add(mainMenuButton,0,6,2,1);
        gridButtons.add(quitGameButton,0,7,2,1);

        /**GRID WITH CELLS**/
        for (int row = 0; row < gridSize; row++) {
            for (int column = 0; column < gridSize; column++) {

                // Style for number in cage
                Label number = new Label(Integer.valueOf(myGrid.gridCells[row][column].getValue()).toString());
                if (myGrid.gridCells[row][column].getValue() == 0) number.setText("");
                number.setFont(numbersFont);
                number.setStyle("-fx-font-weight: bold");
                number.setTextAlignment(TextAlignment.CENTER);

                Pane pane = new Pane(number);
                // style for solution operation
                Label solution = new Label();
                if (myGrid.gridCells[row][column].headCell) {
                    if (myGrid.gridCells[row][column].cage.operation == Cage.operations.DIVISION && myGrid.gridCells[row][column].cage.cellsInCage.size() > 1) {
                        solution = new Label(myGrid.gridCells[row][column].cage.finalSolution + "÷");
                    } else if (myGrid.gridCells[row][column].cage.operation == Cage.operations.MULTIPLICATION && myGrid.gridCells[row][column].cage.cellsInCage.size() > 1) {
                        solution = new Label(myGrid.gridCells[row][column].cage.finalSolution + "×");
                    } else if (myGrid.gridCells[row][column].cage.operation == Cage.operations.SUBTRACTION && myGrid.gridCells[row][column].cage.cellsInCage.size() > 1) {
                        solution = new Label(myGrid.gridCells[row][column].cage.finalSolution + "-");
                    } else if (myGrid.gridCells[row][column].cage.operation == Cage.operations.ADDITION && myGrid.gridCells[row][column].cage.cellsInCage.size() > 1) {
                        solution = new Label(myGrid.gridCells[row][column].cage.finalSolution + "+");
                    } else solution = new Label(myGrid.gridCells[row][column].cage.finalSolution + "");
                    pane.getChildren().add(solution);
                }
                solution.layoutXProperty().bind(pane.widthProperty().subtract(number.widthProperty()).divide(8));
                solution.layoutYProperty().bind(pane.heightProperty().subtract(number.heightProperty()).divide(8));
                solution.setFont(cageFont);
                solution.setTextAlignment(TextAlignment.CENTER);


                number.layoutXProperty().bind(pane.widthProperty().subtract(number.widthProperty()).divide(2));
                number.layoutYProperty().bind(pane.heightProperty().subtract(number.heightProperty()).divide(1.7));

                // handler for when clicking cells
                pane.addEventHandler(MouseEvent.MOUSE_PRESSED, new ClickHandler(myGrid, pane, gridP, number, showMistakesCheckbox));


                pane.setMinSize(100, 100);
                pane.setMaxSize(100, 100);

                // down and right must be thicc
                if (row != gridSize - 1 && !myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row + 1][column].cage) && column != gridSize - 1 && !myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row][column + 1].cage)) {
                    pane.setStyle("-fx-border-color: black black black black ;");
                    pane.setStyle(pane.getStyle() + " -fx-border-width: 1 4 4 1;");
                } else
                    //down must be thicc
                    if (row != gridSize - 1 && !myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row + 1][column].cage)) {
                        pane.setStyle("-fx-border-color: black black black black ;");
                        pane.setStyle(pane.getStyle() + " -fx-border-width: 1 1 4 1;");
                    } else
                        //right must be thicc
                        if (column != gridSize - 1 && !myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row][column + 1].cage)) {
                            pane.setStyle("-fx-border-color: black black black black ;");
                            pane.setStyle(pane.getStyle() + " -fx-border-width: 1 4 1 1;");
                        } else if (row != gridSize - 1 && column != gridSize - 1 && (myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row + 1][column].cage) || myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row][column + 1].cage))) {
                            pane.setStyle("-fx-border-color: black black black black; -fx-border-width: 1; -fx-border-style: solid;");
                        } else if (column == gridSize - 1 && row != gridSize - 1 && myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row + 1][column].cage)) {
                            pane.setStyle("-fx-border-color: black black black black; -fx-border-width: 1; -fx-border-style: solid;");
                        } else if (column == gridSize - 1 && row == gridSize - 1) {
                            pane.setStyle("-fx-border-color: black black black black; -fx-border-width: 1; -fx-border-style: solid;");
                        } else if (row == gridSize - 1 && myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row][column + 1].cage)) {
                            pane.setStyle("-fx-border-color: black black black black; -fx-border-width: 1; -fx-border-style: solid;");
                        }

                /**WHY THE HOLY Hgrfsvfd pane adds first column then row**/
                if (row == 0 && column == 0)
                    pane.setBackground(new Background(new BackgroundFill(new Color(0, 0, 0.125, 0.25), CornerRadii.EMPTY, Insets.EMPTY)));
                else
                    pane.setBackground(new Background(new BackgroundFill(new Color(1, 1, 1, 1), CornerRadii.EMPTY, Insets.EMPTY)));
                gridP.add(pane, column, row);
            }
            // grid pane border
            gridP.setStyle(gridP.getStyle() + "-fx-border-style: solid; -fx-border-width: 3px; -fx-border-color: black;");
        }
        /**GRID WITH CAGES**/

        oneButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, oneButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        twoButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, twoButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        threeButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, threeButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        fourButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, fourButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        fiveButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, fiveButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        sixButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, sixButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        sevenButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, sevenButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        eightButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, eightButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        deleteButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, deleteButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        undoButton.addEventHandler(ActionEvent.ANY, new UndoButtonHandler(myGrid, gridP, showMistakesCheckbox,undoButton,redoButton));
        redoButton.addEventHandler(ActionEvent.ANY, new RedoButtonHandler(myGrid, gridP, showMistakesCheckbox,undoButton,redoButton));
        clearButton.addEventHandler(ActionEvent.ANY, new ClearButtonHandler(myGrid, gridP,undoButton,redoButton));
        showHintButton.addEventHandler(ActionEvent.ANY, new ShowHintButtonHandler(myGrid, gridP, showMistakesCheckbox,undoButton,redoButton));
        showSolutionButton.addEventHandler(ActionEvent.ANY, new ShowSolutionButtonHandler(myGrid, gridP,undoButton,redoButton));

        /**Keyboard entering**/
        gameScene.addEventHandler(KeyEvent.KEY_PRESSED, (key) -> {
            int labelflag = 0;
            int isDelete = 0;
            int sizeofgrid;
            sizeofgrid = myGrid.getSize();
            if ((key.getCode() == KeyCode.BACK_SPACE || key.getCode() == KeyCode.DELETE) && myGrid.currentCell.getValue()!=0) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(0);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
                isDelete = 1;
            } else if ((key.getCode() == KeyCode.DIGIT1 || key.getCode() == KeyCode.NUMPAD1) && sizeofgrid >= 1 && myGrid.currentCell.getValue()!=1) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(1);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT2 || key.getCode() == KeyCode.NUMPAD2) && sizeofgrid >= 2 && myGrid.currentCell.getValue()!=2) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(2);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT3 || key.getCode() == KeyCode.NUMPAD3) && sizeofgrid >= 3 && myGrid.currentCell.getValue()!=3) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(3);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT4 || key.getCode() == KeyCode.NUMPAD4) && sizeofgrid >= 4 && myGrid.currentCell.getValue()!=4) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(4);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT5 || key.getCode() == KeyCode.NUMPAD5) && sizeofgrid >= 5 && myGrid.currentCell.getValue()!=5) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(5);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT6 || key.getCode() == KeyCode.NUMPAD6) && sizeofgrid >= 6 && myGrid.currentCell.getValue()!=6) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(6);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT7 || key.getCode() == KeyCode.NUMPAD7) && sizeofgrid >= 7 && myGrid.currentCell.getValue()!=7) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(7);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT8 || key.getCode() == KeyCode.NUMPAD8) && sizeofgrid >= 8 && myGrid.currentCell.getValue()!=8) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(8);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT9 || key.getCode() == KeyCode.NUMPAD9) && sizeofgrid >= 9 && myGrid.currentCell.getValue()!=9) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(9);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else key.consume();

            // changes the number in the label
            for (Node cell : gridP.getChildren()) {
                if (GridPane.getRowIndex(cell) == myGrid.currentCell.getRow() && GridPane.getColumnIndex(cell) == myGrid.currentCell.getColumn()) {
                    labelflag = 0;
                    if (cell instanceof Pane) {
                        for (Node label : ((Pane) cell).getChildren()) {
                            if (label instanceof Label) {
                                if (labelflag == 0) {
                                    if (myGrid.currentCell.getValue() != 0)
                                        ((Label) label).setText(myGrid.currentCell.getValue() + "");
                                    labelflag = 1;
                                    if (isDelete == 1) ((Label) label).setText("");
                                }
                            }
                        }
                    }
                }
            }

            if(myGrid.canRedo())redoButton.setDisable(false);
            else redoButton.setDisable(true);
            if(myGrid.canUndo())undoButton.setDisable(false);
            else undoButton.setDisable(true);

            //checks if won
            if (myGrid.checkWin()) {
                Random rand = new Random();
                for (Node nodeIn : gridP.getChildren()) {
                    if (nodeIn instanceof Pane) {
                        float r = rand.nextFloat();
                        float g = rand.nextFloat();
                        float b = rand.nextFloat();
                        Color color = new Color(r, g, b, 0.5);
                        ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                    }
                }


                Timeline timeline = new Timeline(new KeyFrame(Duration.millis(45), event -> {
                    for (Node nodeIn : gridP.getChildren()) {
                        if (GridPane.getRowIndex(nodeIn) == rand.nextInt(gridSize) && GridPane.getColumnIndex(nodeIn) == rand.nextInt(gridSize)) {
                            if (nodeIn instanceof Pane) {
                                float r = rand.nextFloat();
                                float g = rand.nextFloat();
                                float b = rand.nextFloat();
                                Color color = new Color(r, g, b, 0.5);
                                ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                    }
                }));
                timeline.setCycleCount(Animation.INDEFINITE);
                timeline.play();

                /**End Prompt**/
                if(hintCounter<=gridSize-2) {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setTitle("YOU WIN!!!");
                    alert.setHeaderText("CONGRATULATIONS YOU WIN!\nYou have used " + hintCounter + " hints.");
                    alert.setContentText("Press OK to go back to main menu.");
                    Optional<ButtonType> result = alert.showAndWait();
                    if (result.isPresent() && result.get() == ButtonType.OK) {
                        mainstage.close();
                    }
                    startMenu();
                }else if(hintCounter>gridSize-2) {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setTitle("YOU WIN!!!");
                    alert.setHeaderText("The cake is a lie!\nYou have used " + hintCounter + " hints.");
                    alert.setContentText("Press OK to go back to main menu.");
                    Optional<ButtonType> result = alert.showAndWait();
                    if (result.isPresent() && result.get() == ButtonType.OK) {
                        mainstage.close();
                    }
                    startMenu();
                }
            }
        });

        /**Checkbox for mistakes**/
        showMistakesCheckbox.setOnAction(e -> {
            Color transparent = new Color(1, 1, 1, 1);
            Color color = new Color(0, 0, 0.125, 0.25);
            if(showMistakesCheckbox.isSelected()) this.ShowMistakes(myGrid,gridP);
            else for (Node nodeIn : gridP.getChildren()) {
                if (GridPane.getRowIndex(nodeIn) == myGrid.currentCell.getRow() && GridPane.getColumnIndex(nodeIn) == myGrid.currentCell.getColumn()) {
                    if (nodeIn instanceof Pane) {
                        ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                    }
                } else if (nodeIn instanceof Pane) {
                    ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(transparent, CornerRadii.EMPTY, Insets.EMPTY)));
                }
            }
        });
        mainMenuButton.setOnAction(e -> {
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
            alert.setTitle("Go back to main menu?");
            alert.setHeaderText("Do you really want to go back to main menu?\nYour progress won't be saved!");
            alert.setContentText(null);
            Optional<ButtonType> result = alert.showAndWait();
            if (result.isPresent() && result.get() == ButtonType.OK) {startMenu();}
            });
        quitGameButton.setOnAction(e -> {
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
            alert.setTitle("Quit?");
            alert.setHeaderText("Do you really want to quit MathDoku\nYour progress won't be saved!");
            alert.setContentText(null);
            Optional<ButtonType> result = alert.showAndWait();
            if (result.isPresent() && result.get() == ButtonType.OK) {mainstage.close();}
        });

        this.mainstage.setScene(gameScene);
        this.mainstage.show();
    }

    // overloading for file grid
    public void createGame(int gridSize,Grid myGrid){
        int width=225+100*gridSize;
        int height=100+100*gridSize;

        GridPane gridP = new GridPane();
        GridPane gridButtons = new GridPane();
        gridButtons.setHgap(5);
        gridButtons.setVgap(5);

        Pane insidePane = new Pane();
        BorderPane borderP = new BorderPane();
        borderP.setBackground(new Background(new BackgroundFill(backColor, CornerRadii.EMPTY, Insets.EMPTY)));

        borderP.setPadding(new Insets(10));
        HBox buttonBox = new HBox(5);
        buttonBox.setAlignment(Pos.CENTER);
        if(gridSize==2){
            width=415;
            height=350;
            buttonBox.setAlignment(Pos.TOP_LEFT);
        }
        if(gridSize==3){
            width=525;
            height=390;
            buttonBox.setAlignment(Pos.TOP_LEFT);
        }

        Scene gameScene = new Scene(borderP, width, height);


        Button undoButton = new Button("Undo");
        Button redoButton = new Button("Redo");
        undoButton.setDisable(true);
        redoButton.setDisable(true);

        Button clearButton = new Button("Clear Board");
        Button showHintButton = new Button("Hint");
        Button showSolutionButton = new Button("Show Solution");
        CheckBox showMistakesCheckbox = new CheckBox("Show Mistakes");
        showMistakesCheckbox.setPadding(new Insets(3));
        Button oneButton = new Button("1");
        Button twoButton = new Button("2");
        Button threeButton = new Button("3");
        Button fourButton = new Button("4");
        Button fiveButton = new Button("5");
        Button sixButton = new Button("6");
        Button sevenButton = new Button("7");
        Button eightButton = new Button("8");
        Button deleteButton = new Button("Delete");
        Button mainMenuButton = new Button("Main Menu");
        Button quitGameButton = new Button("Quit Game");

        /**Button styles**/
        oneButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        twoButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        threeButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        fourButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        fiveButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        sixButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        sevenButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        eightButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffffff; -fx-font-size: 20px;");
        deleteButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffb3b3; -fx-font-size: 20px;");


        undoButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        redoButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        clearButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        showHintButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        showSolutionButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        showHintButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        mainMenuButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        quitGameButton.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        showMistakesCheckbox.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
        undoButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffd480; -fx-font-size: 20px;");
        redoButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffd480; -fx-font-size: 20px;");
        clearButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #f2f2f2; -fx-font-size: 20px;");
        showHintButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #99ff99; -fx-font-size: 20px;");
        showMistakesCheckbox.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffd480; -fx-font-size: 20px;");
        showSolutionButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #66ff66; -fx-font-size: 20px;");
        mainMenuButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffd480; -fx-font-size: 20px;");
        quitGameButton.setStyle("-fx-border-color: #ff0000; -fx-border-width: 3px; -fx-background-color: #ffb3b3; -fx-font-size: 20px;");


        insidePane.getChildren().add(gridP);
        borderP.setCenter(insidePane);
        borderP.setRight(gridButtons);
        borderP.setBottom(buttonBox);

        gridButtons.add(undoButton, 0, 0, 1, 1);
        gridButtons.add(redoButton, 1, 0, 1, 1);
        gridButtons.add(clearButton, 0, 1, 2, 1);
        gridButtons.add(showMistakesCheckbox, 0, 3, 2, 1);
        if (gridSize >= 1) buttonBox.getChildren().add(oneButton);
        if (gridSize >= 2) buttonBox.getChildren().add(twoButton);
        if (gridSize >= 3) buttonBox.getChildren().add(threeButton);
        if (gridSize >= 4) buttonBox.getChildren().add(fourButton);
        if (gridSize >= 5) buttonBox.getChildren().add(fiveButton);
        if (gridSize >= 6) buttonBox.getChildren().add(sixButton);
        if (gridSize >= 7) buttonBox.getChildren().add(sevenButton);
        if (gridSize >= 8) buttonBox.getChildren().add(eightButton);
        buttonBox.getChildren().add(deleteButton);
        gridButtons.add(mainMenuButton,0,4,2,1);
        gridButtons.add(quitGameButton,0,5,2,1);

        /**GRID WITH CELLS**/
        for (int row = 0; row < gridSize; row++) {
            for (int column = 0; column < gridSize; column++) {

                // Style for number in cage
                Label number = new Label(Integer.valueOf(myGrid.gridCells[row][column].getValue()).toString());
                if (myGrid.gridCells[row][column].getValue() == 0) number.setText("");
                number.setFont(numbersFont);
                number.setStyle("-fx-font-weight: bold");
                number.setTextAlignment(TextAlignment.CENTER);

                Pane pane = new Pane(number);
                // style for solution operation
                Label solution = new Label();
                if (myGrid.gridCells[row][column].headCell) {
                    if (myGrid.gridCells[row][column].cage.operation == Cage.operations.DIVISION && myGrid.gridCells[row][column].cage.cellsInCage.size() > 1) {
                        solution = new Label(myGrid.gridCells[row][column].cage.finalSolution + "÷");
                    } else if (myGrid.gridCells[row][column].cage.operation == Cage.operations.MULTIPLICATION && myGrid.gridCells[row][column].cage.cellsInCage.size() > 1) {
                        solution = new Label(myGrid.gridCells[row][column].cage.finalSolution + "×");
                    } else if (myGrid.gridCells[row][column].cage.operation == Cage.operations.SUBTRACTION && myGrid.gridCells[row][column].cage.cellsInCage.size() > 1) {
                        solution = new Label(myGrid.gridCells[row][column].cage.finalSolution + "-");
                    } else if (myGrid.gridCells[row][column].cage.operation == Cage.operations.ADDITION && myGrid.gridCells[row][column].cage.cellsInCage.size() > 1) {
                        solution = new Label(myGrid.gridCells[row][column].cage.finalSolution + "+");
                    } else solution = new Label(myGrid.gridCells[row][column].cage.finalSolution + "");
                    pane.getChildren().add(solution);
                }
                solution.layoutXProperty().bind(pane.widthProperty().subtract(number.widthProperty()).divide(8));
                solution.layoutYProperty().bind(pane.heightProperty().subtract(number.heightProperty()).divide(8));
                solution.setFont(cageFont);
                solution.setTextAlignment(TextAlignment.CENTER);


                number.layoutXProperty().bind(pane.widthProperty().subtract(number.widthProperty()).divide(2));
                number.layoutYProperty().bind(pane.heightProperty().subtract(number.heightProperty()).divide(1.7));

                // handler for when clicking cells
                pane.addEventHandler(MouseEvent.MOUSE_PRESSED, new ClickHandler(myGrid, pane, gridP, number, showMistakesCheckbox));


                pane.setMinSize(100, 100);
                pane.setMaxSize(100, 100);

                // down and right must be thicc
                if (row != gridSize - 1 && !myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row + 1][column].cage) && column != gridSize - 1 && !myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row][column + 1].cage)) {
                    pane.setStyle("-fx-border-color: black black black black ;");
                    pane.setStyle(pane.getStyle() + " -fx-border-width: 1 4 4 1;");
                } else
                    //down must be thicc
                    if (row != gridSize - 1 && !myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row + 1][column].cage)) {
                        pane.setStyle("-fx-border-color: black black black black ;");
                        pane.setStyle(pane.getStyle() + " -fx-border-width: 1 1 4 1;");
                    } else
                        //right must be thicc
                        if (column != gridSize - 1 && !myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row][column + 1].cage)) {
                            pane.setStyle("-fx-border-color: black black black black ;");
                            pane.setStyle(pane.getStyle() + " -fx-border-width: 1 4 1 1;");
                        } else if (row != gridSize - 1 && column != gridSize - 1 && (myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row + 1][column].cage) || myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row][column + 1].cage))) {
                            pane.setStyle("-fx-border-color: black black black black; -fx-border-width: 1; -fx-border-style: solid;");
                        } else if (column == gridSize - 1 && row != gridSize - 1 && myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row + 1][column].cage)) {
                            pane.setStyle("-fx-border-color: black black black black; -fx-border-width: 1; -fx-border-style: solid;");
                        } else if (column == gridSize - 1 && row == gridSize - 1) {
                            pane.setStyle("-fx-border-color: black black black black; -fx-border-width: 1; -fx-border-style: solid;");
                        } else if (row == gridSize - 1 && myGrid.gridCells[row][column].cage.equals(myGrid.gridCells[row][column + 1].cage)) {
                            pane.setStyle("-fx-border-color: black black black black; -fx-border-width: 1; -fx-border-style: solid;");
                        }

                /**WHY THE HOLY Hgrfsvfd pane adds first column then row**/
                if (row == 0 && column == 0)
                    pane.setBackground(new Background(new BackgroundFill(new Color(0, 0, 0.125, 0.25), CornerRadii.EMPTY, Insets.EMPTY)));
                else
                    pane.setBackground(new Background(new BackgroundFill(new Color(1, 1, 1, 1), CornerRadii.EMPTY, Insets.EMPTY)));
                gridP.add(pane, column, row);
            }
            // grid pane border
            gridP.setStyle(gridP.getStyle() + "-fx-border-style: solid; -fx-border-width: 3px; -fx-border-color: black;");
        }
        /**GRID WITH CAGES**/

        oneButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, oneButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        twoButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, twoButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        threeButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, threeButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        fourButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, fourButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        fiveButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, fiveButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        sixButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, sixButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        sevenButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, sevenButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        eightButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, eightButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        deleteButton.addEventHandler(ActionEvent.ANY, new NumberButtonHandler(myGrid, gridP, deleteButton.getText(), showMistakesCheckbox,undoButton,redoButton));
        undoButton.addEventHandler(ActionEvent.ANY, new UndoButtonHandler(myGrid, gridP, showMistakesCheckbox,undoButton,redoButton));
        redoButton.addEventHandler(ActionEvent.ANY, new RedoButtonHandler(myGrid, gridP, showMistakesCheckbox,undoButton,redoButton));
        clearButton.addEventHandler(ActionEvent.ANY, new ClearButtonHandler(myGrid, gridP,undoButton,redoButton));
        showHintButton.addEventHandler(ActionEvent.ANY, new ShowHintButtonHandler(myGrid, gridP, showMistakesCheckbox,undoButton,redoButton));
        showSolutionButton.addEventHandler(ActionEvent.ANY, new ShowSolutionButtonHandler(myGrid, gridP,undoButton,redoButton));

        /**Keyboard entering**/
        gameScene.addEventHandler(KeyEvent.KEY_PRESSED, (key) -> {
            int labelflag = 0;
            int isDelete = 0;
            int sizeofgrid;
            sizeofgrid = myGrid.getSize();
            if ((key.getCode() == KeyCode.BACK_SPACE || key.getCode() == KeyCode.DELETE) && myGrid.currentCell.getValue()!=0) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(0);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
                isDelete = 1;
            } else if ((key.getCode() == KeyCode.DIGIT1 || key.getCode() == KeyCode.NUMPAD1) && sizeofgrid >= 1 && myGrid.currentCell.getValue()!=1) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(1);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT2 || key.getCode() == KeyCode.NUMPAD2) && sizeofgrid >= 2 && myGrid.currentCell.getValue()!=2) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(2);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT3 || key.getCode() == KeyCode.NUMPAD3) && sizeofgrid >= 3 && myGrid.currentCell.getValue()!=3) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(3);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT4 || key.getCode() == KeyCode.NUMPAD4) && sizeofgrid >= 4 && myGrid.currentCell.getValue()!=4) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(4);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT5 || key.getCode() == KeyCode.NUMPAD5) && sizeofgrid >= 5 && myGrid.currentCell.getValue()!=5) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(5);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT6 || key.getCode() == KeyCode.NUMPAD6) && sizeofgrid >= 6 && myGrid.currentCell.getValue()!=6) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(6);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT7 || key.getCode() == KeyCode.NUMPAD7) && sizeofgrid >= 7 && myGrid.currentCell.getValue()!=7) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(7);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT8 || key.getCode() == KeyCode.NUMPAD8) && sizeofgrid >= 8 && myGrid.currentCell.getValue()!=8) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(8);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else if ((key.getCode() == KeyCode.DIGIT9 || key.getCode() == KeyCode.NUMPAD9) && sizeofgrid >= 9 && myGrid.currentCell.getValue()!=9) {
                myGrid.undoAdd(myGrid.currentCell);
                myGrid.currentCell.setValue(9);
                if(showMistakesCheckbox.isSelected())this.ShowMistakes(myGrid,gridP);
                myGrid.undoAdd(myGrid.currentCell);
            } else key.consume();

            // changes the number in the label
            for (Node cell : gridP.getChildren()) {
                if (GridPane.getRowIndex(cell) == myGrid.currentCell.getRow() && GridPane.getColumnIndex(cell) == myGrid.currentCell.getColumn()) {
                    labelflag = 0;
                    if (cell instanceof Pane) {
                        for (Node label : ((Pane) cell).getChildren()) {
                            if (label instanceof Label) {
                                if (labelflag == 0) {
                                    if (myGrid.currentCell.getValue() != 0)
                                        ((Label) label).setText(myGrid.currentCell.getValue() + "");
                                    labelflag = 1;
                                    if (isDelete == 1) ((Label) label).setText("");
                                }
                            }
                        }
                    }
                }
            }

            if(myGrid.canRedo())redoButton.setDisable(false);
            else redoButton.setDisable(true);
            if(myGrid.canUndo())undoButton.setDisable(false);
            else undoButton.setDisable(true);

            //checks if won
            if (myGrid.checkWin()) {
                Random rand = new Random();
                for (Node nodeIn : gridP.getChildren()) {
                    if (nodeIn instanceof Pane) {
                        float r = rand.nextFloat();
                        float g = rand.nextFloat();
                        float b = rand.nextFloat();
                        Color color = new Color(r, g, b, 0.5);
                        ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                    }
                }


                Timeline timeline = new Timeline(new KeyFrame(Duration.millis(45), event -> {
                    for (Node nodeIn : gridP.getChildren()) {
                        if (GridPane.getRowIndex(nodeIn) == rand.nextInt(gridSize) && GridPane.getColumnIndex(nodeIn) == rand.nextInt(gridSize)) {
                            if (nodeIn instanceof Pane) {
                                float r = rand.nextFloat();
                                float g = rand.nextFloat();
                                float b = rand.nextFloat();
                                Color color = new Color(r, g, b, 0.5);
                                ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                    }
                }));
                timeline.setCycleCount(Animation.INDEFINITE);
                timeline.play();

                /**End Prompt**/
                if(hintCounter<=gridSize-2) {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setTitle("YOU WIN!!!");
                    alert.setHeaderText("CONGRATULATIONS YOU WIN!\nYou have used " + hintCounter + " hints.");
                    alert.setContentText("Press OK to go back to main menu.");
                    Optional<ButtonType> result = alert.showAndWait();
                    if (result.isPresent() && result.get() == ButtonType.OK) {
                        mainstage.close();
                    }
                    startMenu();
                }else if(hintCounter>gridSize-2) {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setTitle("YOU WIN!!!");
                    alert.setHeaderText("The cake is a lie!\nYou have used " + hintCounter + " hints.");
                    alert.setContentText("Press OK to go back to main menu.");
                    Optional<ButtonType> result = alert.showAndWait();
                    if (result.isPresent() && result.get() == ButtonType.OK) {
                        mainstage.close();
                    }
                    startMenu();
                }
            }
        });

        /**Checkbox for mistakes**/
        showMistakesCheckbox.setOnAction(e -> {
            Color transparent = new Color(1, 1, 1, 1);
            Color color = new Color(0, 0, 0.125, 0.25);
            if(showMistakesCheckbox.isSelected()) this.ShowMistakes(myGrid,gridP);
            else for (Node nodeIn : gridP.getChildren()) {
                if (GridPane.getRowIndex(nodeIn) == myGrid.currentCell.getRow() && GridPane.getColumnIndex(nodeIn) == myGrid.currentCell.getColumn()) {
                    if (nodeIn instanceof Pane) {
                        ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                    }
                } else if (nodeIn instanceof Pane) {
                    ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(transparent, CornerRadii.EMPTY, Insets.EMPTY)));
                }
            }
        });
        mainMenuButton.setOnAction(e -> {
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
            alert.setTitle("Go back to main menu?");
            alert.setHeaderText("Do you really want to go back to main menu?\nYour progress won't be saved!");
            alert.setContentText(null);
            Optional<ButtonType> result = alert.showAndWait();
            if (result.isPresent() && result.get() == ButtonType.OK) {startMenu();}
            });
        quitGameButton.setOnAction(e -> {
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
            alert.setTitle("Quit?");
            alert.setHeaderText("Do you really want to quit MathDoku\nYour progress won't be saved!");
            alert.setContentText(null);
            Optional<ButtonType> result = alert.showAndWait();
            if (result.isPresent() && result.get() == ButtonType.OK) {mainstage.close();}
        });

        this.mainstage.setScene(gameScene);
        this.mainstage.show();
    }

    /**Checkbox for mistakes**/
    public void ShowMistakes(Grid grid, GridPane gridPane){
        Label label = null;
        Pane pane;
        String number;
        int labelflag = 0;
        int emptyflag=0;
        Color color = new Color(0, 0, 0.125, 0.25);
        Color duplicates = new Color(1, 0.5, 0.5, 1);
        Color colorWrongCage = new Color(1, 0.8, 0.8, 1);
        Color colorWrongSelected = new Color(0.7, 0.5, 0.5, 1);
        Color transparent = new Color(1, 1, 1, 1);

        for (Node nodeIn : gridPane.getChildren()) {
            if (GridPane.getRowIndex(nodeIn) == grid.currentCell.getRow() && GridPane.getColumnIndex(nodeIn) == grid.currentCell.getColumn()) {
                if (nodeIn instanceof Pane) {
                    ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                }
            } else if (nodeIn instanceof Pane) {
                ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(transparent, CornerRadii.EMPTY, Insets.EMPTY)));
            }
        }
        // checks for mistaken solutions in cages and highlights the whole wrong cages
        for (Cage cage: grid.cagelist) {
            emptyflag=0;
            for (Cell checkcell: cage.cellsInCage) {
                if(checkcell.getValue()==0) emptyflag=1;
            }
            if(emptyflag==1) continue;
            if(!cage.checkSolution()) {
                for (Cell cagecell : cage.cellsInCage) {
                    for (Node cell : gridPane.getChildren()) {
                        if(GridPane.getRowIndex(cell) == cagecell.getRow() && GridPane.getColumnIndex(cell) == cagecell.getColumn() && grid.currentCell.getRow()== cagecell.getRow() && grid.currentCell.getColumn()== cagecell.getColumn()){
                            labelflag = 0;
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(colorWrongSelected, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                        else if (GridPane.getRowIndex(cell) == cagecell.getRow() && GridPane.getColumnIndex(cell) == cagecell.getColumn()) {
                            labelflag = 0;
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(colorWrongCage, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                    }
                }
            }
        }

        // Checking for duplicate numbers on rows and columns
        ArrayList<Integer> cellValuesRow = new ArrayList<>();
        ArrayList<Integer> cellValuesColumn = new ArrayList<>();

        // row and column duplicate check
        for(int i=0;i<grid.getSize();i++) {
            for (int j = 0; j <grid.getSize(); j++){
                if(grid.gridCells[i][j].getValue()!=0) cellValuesRow.add(grid.gridCells[i][j].getValue());
                if(grid.gridCells[j][i].getValue()!=0) cellValuesColumn.add(grid.gridCells[j][i].getValue());
            }
            for (int k = 1; k <= grid.getSize(); k++) {
                if (cellValuesRow.indexOf(k) != cellValuesRow.lastIndexOf(k)) {
                    for (Node cell : gridPane.getChildren()) {
                        if(GridPane.getRowIndex(cell) == i && grid.currentCell.getRow()== i && GridPane.getRowIndex(cell)==grid.currentCell.getRow() && GridPane.getColumnIndex(cell)==grid.currentCell.getColumn()){
                            labelflag = 0;
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(colorWrongSelected, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        } else if(GridPane.getRowIndex(cell) == i){
                            labelflag = 0;
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(duplicates, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                    }
                }
                if (cellValuesColumn.indexOf(k) != cellValuesColumn.lastIndexOf(k)) {
                    for (Node cell : gridPane.getChildren()) {
                        if(GridPane.getColumnIndex(cell) == i && grid.currentCell.getColumn()== i && GridPane.getRowIndex(cell)==grid.currentCell.getRow() && GridPane.getColumnIndex(cell)==grid.currentCell.getColumn()){
                            labelflag = 0;
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(colorWrongSelected, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        } else if(GridPane.getColumnIndex(cell) == i){
                            labelflag = 0;
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(duplicates, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                    }
                }
            }
            cellValuesRow.clear();
            cellValuesColumn.clear();
        }
    }

    // combines the rest of the random gen methods
    public static void makeRandomGame(Grid grid, int size) {
        grid.randGridSolution = makeRandGrid(size);
        makeRandCages(grid, makeCagesNumbers(size), size);
    }

    public static Cell[][] makeRandGrid(int size) {
        Cell[][] randGridSolution = new Cell[size][size];
        ArrayList<Integer> numbersAll = new ArrayList<>();
        Random rnd = new Random();

        // fills numbersAll for each line
        for (int number = 1; number <= size; number++) {
            numbersAll.add(number);
        }
        Collections.shuffle(numbersAll);

        ArrayList<Integer> numbersLeft;

        for (int row = 0; row < size; row++) {
            for (int column = 0; column < size; column++) {
                numbersLeft = new ArrayList<>(numbersAll);
                // fills randomly shuffled first row
                if (row == 0) randGridSolution[row][column] = new Cell(numbersLeft.get(column), row, column);
                else {
                    for (int i = 0; i < row; i++) {
                        if (numbersLeft.contains(randGridSolution[i][column].getValue()))
                            numbersLeft.remove(Integer.valueOf(randGridSolution[i][column].getValue()));
                    }
                    for (int j = 0; j < column; j++) {
                        if (numbersLeft.contains(randGridSolution[row][j].getValue()))
                            numbersLeft.remove(Integer.valueOf(randGridSolution[row][j].getValue()));
                    }
                    if (numbersLeft.size() == 0) {
                        row--;
                        break;
                    } else
                        randGridSolution[row][column] = new Cell(numbersLeft.get(rnd.nextInt(numbersLeft.size())), row, column);
                }
            }
        }
        return randGridSolution;
    }

    public static ArrayList<ArrayList<Integer>> makeCagesNumbers(int n) {

        //all of the cellNumbersAll
        ArrayList<Integer> cellNumbersAll = new ArrayList<>();
        Random rand = new Random();
        int randomValue = 0;
        ArrayList<ArrayList<Integer>> cageList = new ArrayList<>();
        ArrayList<Integer> numbersInCage = new ArrayList<>();
        int reachedNumber = 1;
        for (int number = 1; number <= n * n; number++) {
            cellNumbersAll.add(number);
        }

        // goes through all the cell numbers
        while (!cellNumbersAll.isEmpty()) {
            reachedNumber = cellNumbersAll.get(0);
            //numbersInCage.add(reachedNumber);
            int cageCount = 2;
            randomValue = rand.nextInt(100);
            if (randomValue < 100 && randomValue >= 95 && n > 2) cageCount = 5;
            else if (randomValue < 95 && randomValue >= 75 && n > 2) cageCount = 4;
            else if (randomValue < 75 && randomValue >= 40 && n > 2) cageCount = 3;
            else if (randomValue < 40 && randomValue >= 0 && n > 2) cageCount = 2;


            for (int i = 1; i <= cageCount; i++) {
                randomValue = rand.nextInt(100);
                // add left and down element
                if (randomValue >= 90 && reachedNumber % n != 0 && cellNumbersAll.contains(reachedNumber + n) && cellNumbersAll.contains(reachedNumber + 1)) {
                    if (rand.nextInt(1) == 0) {
                        numbersInCage.add(reachedNumber);
                        cellNumbersAll.remove(Integer.valueOf(reachedNumber));
                        numbersInCage.add(reachedNumber + n);
                        cellNumbersAll.remove(Integer.valueOf(reachedNumber + n));
                        reachedNumber++;
                    } else {
                        numbersInCage.add(reachedNumber);
                        cellNumbersAll.remove(Integer.valueOf(reachedNumber));
                        numbersInCage.add(reachedNumber + 1);
                        cellNumbersAll.remove(Integer.valueOf(reachedNumber + 1));
                        reachedNumber = reachedNumber + n;
                    }
                    continue;
                } else randomValue = rand.nextInt(90);

                if (randomValue < 90) {
                    // add down element
                    if (randomValue >= 45 && randomValue < 90 && cellNumbersAll.contains(reachedNumber + n)) {
                        numbersInCage.add(reachedNumber);
                        cellNumbersAll.remove(Integer.valueOf(reachedNumber));
                        reachedNumber = reachedNumber + n;
                        continue;
                    } else {
                        //add element right
                        if (reachedNumber % n != 0 && cellNumbersAll.contains(reachedNumber + 1)) {
                            numbersInCage.add(reachedNumber);
                            cellNumbersAll.remove(Integer.valueOf(reachedNumber));
                            reachedNumber++;
                            continue;
                        }
                    }
                }
                // add down element if right doesn't work
                if (cellNumbersAll.contains(reachedNumber + n)) {
                    numbersInCage.add(reachedNumber);
                    cellNumbersAll.remove(Integer.valueOf(reachedNumber));
                    reachedNumber = reachedNumber + n;
                    continue;
                }
                numbersInCage.add(reachedNumber);
                cellNumbersAll.remove(Integer.valueOf(reachedNumber));
                break;
            }
            cageList.add(numbersInCage);
            numbersInCage = new ArrayList<>();
        }
        return cageList;
    }

    public static void makeRandCages(Grid grid, ArrayList<ArrayList<Integer>> cagesGivenNumber, int size) {
        ArrayList<Cage> cages = new ArrayList<>();
        int addition;
        int multiplication;
        int reachedCellValue;
        int maximum;
        int headflag;
        Random random = new Random();
        int randomcase;

        for (ArrayList<Integer> arrays : cagesGivenNumber) {
            addition = 0;
            multiplication = 1;
            maximum = 0;
            headflag = 1;

            for (Integer value : arrays) {
                // makes first cell of cage the main cell for later label
                if (headflag == 1) {
                    grid.gridCells[(value - 1) / size][(value - 1) % size].headCell = true;
                    headflag = 0;
                }
                reachedCellValue = grid.randGridSolution[(value - 1) / size][(value - 1) % size].getValue();
                multiplication = multiplication * reachedCellValue;
                addition = addition + reachedCellValue;
                if (reachedCellValue > maximum) maximum = reachedCellValue;
            }
            randomcase = random.nextInt(6);
            // plus
            if (randomcase == 0) {
                cages.add(new Cage(addition, Cage.operations.ADDITION, arrays, size));
            }
            // minus
            if ((randomcase == 1 || randomcase == 2) && 2 * maximum - addition >= 0) {
                cages.add(new Cage(2 * maximum - addition, Cage.operations.SUBTRACTION, arrays, size));
            } else if (randomcase == 2 && multiplication < 500) {
                cages.add(new Cage(multiplication, Cage.operations.MULTIPLICATION, arrays, size));
            } else if (randomcase == 1 || randomcase == 2) {
                cages.add(new Cage(addition, Cage.operations.ADDITION, arrays, size));
            }
            // multiplication
            if (randomcase == 3 && multiplication < 500) {
                cages.add(new Cage(multiplication, Cage.operations.MULTIPLICATION, arrays, size));
            } else if (randomcase == 3) {
                cages.add(new Cage(addition, Cage.operations.ADDITION, arrays, size));
            }
            //division
            if ((randomcase == 4 || randomcase == 5) && (maximum * maximum) % multiplication == 0) {
                cages.add(new Cage((maximum * maximum) / multiplication, Cage.operations.DIVISION, arrays, size));
            } else if (randomcase == 5 && multiplication < 500) {
                cages.add(new Cage(multiplication, Cage.operations.MULTIPLICATION, arrays, size));
            } else if (randomcase == 4) {
                cages.add(new Cage(addition, Cage.operations.ADDITION, arrays, size));
            }
            if(cages.size()==0){
                cages.add(new Cage(addition, Cage.operations.ADDITION, arrays, size));
            }

            // adds the cage to the cells in it
            for (Integer coord : arrays) {
                if(cages.size()-1<0)makeRandomGame(grid,size);
                grid.gridCells[(coord - 1) / size][(coord - 1) % size].cage = cages.get(cages.size() - 1);
                grid.randGridSolution[(coord - 1) / size][(coord - 1) % size].cage = cages.get(cages.size() - 1);
            }
        }
        grid.cagelist = cages;
        for (Cage cage : cages) {
            cage.setGrid(grid);
        }
    }

    // selected cell click
    class ClickHandler implements EventHandler<MouseEvent> {

        Label label = null;
        Grid grid;
        Pane pane;
        GridPane gridPane;
        Color color = new Color(0, 0, 0.125, 0.25);
        Color transparent = new Color(1, 1, 1, 1);
        CheckBox checkB;

        public ClickHandler(Grid grid, Pane pane, GridPane gridPane, Label label, CheckBox checkBox) {
            this.label = label;
            this.grid = grid;
            this.pane = pane;
            this.gridPane = gridPane;
            this.checkB=checkBox;
        }

        public void handle(MouseEvent event) {
            for (Node nodeIn : gridPane.getChildren()) {
                if (nodeIn instanceof Pane) {
                    ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(transparent, CornerRadii.EMPTY, Insets.EMPTY)));
                }
            }
            pane.setBackground(new Background(new BackgroundFill(transparent, CornerRadii.EMPTY, Insets.EMPTY)));
            Node source = (Node) event.getSource();
            Integer colIndex = GridPane.getColumnIndex(source);
            Integer rowIndex = GridPane.getRowIndex(source);

            this.grid.currentCell = this.grid.gridCells[rowIndex][colIndex];
            pane.setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));

            if(checkB.isSelected())ShowMistakes(grid,gridPane);
        }
    }

    // number buttons and delete
    class NumberButtonHandler implements EventHandler<ActionEvent> {

        Label label = null;
        Grid grid;
        Pane pane;
        GridPane gridPane;
        String number;
        int labelflag = 0;
        int isDelete = 0;
        CheckBox checkB;
        Button redo;
        Button undo;

        public NumberButtonHandler(Grid grid, GridPane gridPane, String number, CheckBox checkBox, Button undoButton, Button redoButton) {
            this.label = label;
            this.grid = grid;
            this.pane = pane;
            this.gridPane = gridPane;
            this.number = number;
            this.checkB= checkBox;
            this.redo=redoButton;
            this.undo=undoButton;
        }

        @Override
        public void handle(ActionEvent actionEvent) {
            if (!number.equals("Delete")) {
                if(grid.currentCell.getValue()!=Integer.parseInt(number)){
                    grid.undoAdd(grid.currentCell);
                    grid.currentCell.setValue(Integer.parseInt(number));
                    grid.undoAdd(grid.currentCell);
                }
            }
            else {
                if(grid.currentCell.getValue()!=0){
                    grid.undoAdd(grid.currentCell);
                    grid.currentCell.setValue(0);
                    grid.undoAdd(grid.currentCell);
                }
                isDelete = 1;
            }

            for (Node cell : gridPane.getChildren()) {
                if (GridPane.getRowIndex(cell) == grid.currentCell.getRow() && GridPane.getColumnIndex(cell) == grid.currentCell.getColumn()) {
                    labelflag = 0;
                    if (cell instanceof Pane) {
                        for (Node label : ((Pane) cell).getChildren()) {
                            if (label instanceof Label) {
                                if (labelflag == 0) {
                                    ((Label) label).setText(number);
                                    labelflag = 1;
                                    if (isDelete == 1) ((Label) label).setText("");
                                }
                            }
                        }
                    }
                }
            }
            if(checkB.isSelected())ShowMistakes(grid,gridPane);
            if(grid.canRedo())redo.setDisable(false);
            else redo.setDisable(true);
            if(grid.canUndo())undo.setDisable(false);
            else undo.setDisable(true);

            //checks if won
            if (grid.checkWin()) {
                Random rand = new Random();
                for (Node nodeIn : gridPane.getChildren()) {
                    if (nodeIn instanceof Pane) {
                        float r = rand.nextFloat();
                        float g = rand.nextFloat();
                        float b = rand.nextFloat();
                        Color color = new Color(r, g, b, 0.5);
                        ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                    }
                }


                Timeline timeline = new Timeline(new KeyFrame(Duration.millis(50), event -> {
                    for (Node nodeIn : gridPane.getChildren()) {
                        if (GridPane.getRowIndex(nodeIn) == rand.nextInt(grid.getSize()) && GridPane.getColumnIndex(nodeIn) == rand.nextInt(grid.getSize())) {
                            if (nodeIn instanceof Pane) {
                                float r = rand.nextFloat();
                                float g = rand.nextFloat();
                                float b = rand.nextFloat();
                                Color color = new Color(r, g, b, 0.5);
                                ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                    }
                }));
                timeline.setCycleCount(Animation.INDEFINITE);
                timeline.play();

                /**End Prompt**/
                if(hintCounter<=grid.getSize()-2) {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setTitle("YOU WIN!!!");
                    alert.setHeaderText("CONGRATULATIONS YOU WIN!\nYou have used " + hintCounter + " hints.");
                    alert.setContentText("Press OK to go back to main menu.");
                    Optional<ButtonType> result = alert.showAndWait();
                    if (result.isPresent() && result.get() == ButtonType.OK) {
                        mainstage.close();
                    }
                    startMenu();
                }else if(hintCounter>grid.getSize()-2) {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setTitle("YOU WIN!!!");
                    alert.setHeaderText("The cake is a lie!\nYou have used " + hintCounter + " hints.");
                    alert.setContentText("Press OK to go back to main menu.");
                    Optional<ButtonType> result = alert.showAndWait();
                    if (result.isPresent() && result.get() == ButtonType.OK) {
                        mainstage.close();
                    }
                    startMenu();
                }
            }
        }
    }

    // undo button
    class UndoButtonHandler implements EventHandler<ActionEvent> {

        Label label = null;
        Grid grid;
        Pane pane;
        GridPane gridPane;
        String number;
        int labelflag = 0;
        Color color = new Color(0, 0, 0.125, 0.25);
        Color transparent = new Color(1, 1, 1, 1);
        CheckBox checkB;
        Button redo;
        Button undo;

        public UndoButtonHandler(Grid grid, GridPane gridPane, CheckBox checkBox, Button undoButton, Button redoButton) {
            this.label = label;
            this.grid = grid;
            this.pane = pane;
            this.gridPane = gridPane;
            this.number = number;
            this.checkB=checkBox;
            this.redo=redoButton;
            this.undo=undoButton;
        }

        @Override
        public void handle(ActionEvent actionEvent) {
            for(int i=1;i<=2;i++) {
                if (grid.canUndo()) {
                    grid.gridCells[grid.stackUndo.peek().getRow()][grid.stackUndo.peek().getColumn()].setValue(grid.stackUndo.peek().getValue());
                    for (Node cell : gridPane.getChildren()) {
                        if (GridPane.getRowIndex(cell) == grid.stackUndo.peek().getRow() && GridPane.getColumnIndex(cell) == grid.stackUndo.peek().getColumn()) {
                            labelflag = 0;
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                                for (Node label : ((Pane) cell).getChildren()) {
                                    if (label instanceof Label) {
                                        if (labelflag == 0) {
                                            ((Label) label).setText(Integer.toString(grid.stackUndo.peek().getValue()));
                                            labelflag = 1;
                                            if (grid.stackUndo.peek().getValue() == 0) ((Label) label).setText("");
                                        }
                                    }
                                }
                            }
                        }
                        // cleans the colour of the current cell so it only can set the undone cell to the selected cell
                        else if (GridPane.getRowIndex(cell) == grid.currentCell.getRow() && GridPane.getColumnIndex(cell) == grid.currentCell.getColumn()) {
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(transparent, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                    }
                    grid.currentCell = grid.gridCells[grid.stackUndo.peek().getRow()][grid.stackUndo.peek().getColumn()];
                    grid.undoButtonClicked();
                }
            }
            if(checkB.isSelected())ShowMistakes(grid,gridPane);
            if(grid.canRedo())redo.setDisable(false);
            else redo.setDisable(true);
            if(grid.canUndo())undo.setDisable(false);
            else undo.setDisable(true);
        }
    }

    // redo button
    class RedoButtonHandler implements EventHandler<ActionEvent> {

        Label label = null;
        Grid grid;
        Pane pane;
        GridPane gridPane;
        String number;
        int labelflag = 0;
        Color color = new Color(0, 0, 0.125, 0.25);
        Color transparent = new Color(1, 1, 1, 1);
        CheckBox checkB;
        Button redo;
        Button undo;

        public RedoButtonHandler(Grid grid, GridPane gridPane, CheckBox checkBox, Button undoButton, Button redoButton) {
            this.label = label;
            this.grid = grid;
            this.pane = pane;
            this.gridPane = gridPane;
            this.number = number;
            this.checkB=checkBox;
            this.redo=redoButton;
            this.undo=undoButton;
        }

        @Override
        public void handle(ActionEvent actionEvent) {
            for(int i=1;i<=2;i++) {
                if (grid.canRedo()) {
                    grid.gridCells[grid.stackRedo.peek().getRow()][grid.stackRedo.peek().getColumn()].setValue(grid.stackRedo.peek().getValue());
                    for (Node cell : gridPane.getChildren()) {
                        if (GridPane.getRowIndex(cell) == grid.stackRedo.peek().getRow() && GridPane.getColumnIndex(cell) == grid.stackRedo.peek().getColumn()) {
                            labelflag = 0;
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                                for (Node label : ((Pane) cell).getChildren()) {
                                    if (label instanceof Label) {
                                        if (labelflag == 0) {
                                            ((Label) label).setText(Integer.toString(grid.stackRedo.peek().getValue()));
                                            labelflag = 1;
                                            if (grid.stackRedo.peek().getValue() == 0) ((Label) label).setText("");
                                        }
                                    }
                                }
                            }
                        }
                        // cleans the colour of the current cell so it only can set the redone cell to the selected cell
                        else if (GridPane.getRowIndex(cell) == grid.currentCell.getRow() && GridPane.getColumnIndex(cell) == grid.currentCell.getColumn()) {
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(transparent, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                    }
                    grid.currentCell = grid.gridCells[grid.stackRedo.peek().getRow()][grid.stackRedo.peek().getColumn()];
                    grid.redoButtonClicked();
                }
            }
            if(checkB.isSelected())ShowMistakes(grid,gridPane);
            if(grid.canRedo())redo.setDisable(false);
            else redo.setDisable(true);
            if(grid.canUndo())undo.setDisable(false);
            else undo.setDisable(true);
        }
    }

    // clear board
    class ClearButtonHandler implements EventHandler<ActionEvent> {

        Label label = null;
        Grid grid;
        Pane pane;
        GridPane gridPane;
        String number;
        int labelflag = 0;
        Color color = new Color(0, 0, 0.125, 0.25);
        Color transparent = new Color(1, 1, 1, 1);
        Button redo;
        Button undo;


        public ClearButtonHandler(Grid grid, GridPane gridPane, Button undoButton, Button redoButton) {
            this.label = label;
            this.grid = grid;
            this.pane = pane;
            this.gridPane = gridPane;
            this.number = number;
            this.redo=redoButton;
            this.undo=undoButton;
        }

        @Override
        public void handle(ActionEvent actionEvent) {
            for (Node cell : gridPane.getChildren()) {
                if (GridPane.getRowIndex(cell) == 0 && GridPane.getColumnIndex(cell) == 0) {
                    labelflag = 0;
                    if (cell instanceof Pane) {
                        ((Pane) cell).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                        for (Node label : ((Pane) cell).getChildren()) {
                            if (label instanceof Label) {
                                if (labelflag == 0) {
                                    labelflag = 1;
                                    ((Label) label).setText("");
                                }
                            }
                        }
                    }
                }else {
                    labelflag = 0;
                    if (cell instanceof Pane) {
                        ((Pane) cell).setBackground(new Background(new BackgroundFill(transparent, CornerRadii.EMPTY, Insets.EMPTY)));
                        for (Node label : ((Pane) cell).getChildren()) {
                            if (label instanceof Label) {
                                if (labelflag == 0) {
                                    labelflag = 1;
                                    ((Label) label).setText("");
                                }
                            }
                        }
                    }
                }
            }
            grid.clearGrid();
            if(grid.canRedo())redo.setDisable(false);
            else redo.setDisable(true);
            if(grid.canUndo())undo.setDisable(false);
            else undo.setDisable(true);
        }
    }

    // shows a single hint
    class ShowHintButtonHandler implements EventHandler<ActionEvent> {

        Label label = null;
        Grid grid;
        Pane pane;
        GridPane gridPane;
        String number;
        int labelflag = 0;
        int finished = 0;
        Random randomCoord = new Random();
        int randomRow;
        int randomColumn;
        ArrayList<Cell> cells = new ArrayList<>();
        Color color = new Color(0, 0, 0.125, 0.25);
        Color duplicates = new Color(1, 0.5, 0.5, 1);
        Color hinted = new Color(0, 0.75, 0, 1);
        Color colorWrongSelected = new Color(0.7, 0.5, 0.5, 1);
        Color transparent = new Color(1, 1, 1, 1);
        CheckBox checkB;
        Button redo;
        Button undo;

        public ShowHintButtonHandler(Grid grid, GridPane gridPane, CheckBox checkBox, Button undoButton, Button redoButton) {
            this.label = label;
            this.grid = grid;
            this.pane = pane;
            this.gridPane = gridPane;
            this.number = number;
            this.checkB=checkBox;
            this.redo=redoButton;
            this.undo=undoButton;
        }

        @Override
        public void handle(ActionEvent actionEvent) {
            finished = 0;
            cells = new ArrayList<>();
            while(finished==0 && cells.size()<grid.getSize()*grid.getSize()) {
                randomRow = randomCoord.nextInt(grid.getSize());
                randomColumn = randomCoord.nextInt(grid.getSize());
                if(!cells.contains(grid.gridCells[randomRow][randomColumn]) && grid.gridCells[randomRow][randomColumn].getValue()!=grid.randGridSolution[randomRow][randomColumn].getValue()) {
                    for (Node cell : gridPane.getChildren()) {
                        if (GridPane.getRowIndex(cell) == randomRow && GridPane.getColumnIndex(cell) == randomColumn) {
                            labelflag = 0;
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(hinted, CornerRadii.EMPTY, Insets.EMPTY)));
                                for (Node label : ((Pane) cell).getChildren()) {
                                    if (label instanceof Label) {
                                        if (labelflag == 0) {
                                            grid.undoAdd(grid.gridCells[randomRow][randomColumn]);
                                            grid.gridCells[randomRow][randomColumn].setValue(grid.randGridSolution[randomRow][randomColumn].getValue());
                                            grid.undoAdd(grid.gridCells[randomRow][randomColumn]);
                                            ((Label) label).setText(Integer.toString(grid.randGridSolution[randomRow][randomColumn].getValue()));
                                            labelflag = 1;
                                            if (grid.randGridSolution[randomRow][randomColumn].getValue()==0) ((Label) label).setText("");
                                        }
                                    }
                                }
                            }
                        }
                        // cleans the colour of the current cell so it only can set the undone cell to the selected cell
                        else if (GridPane.getRowIndex(cell) == grid.currentCell.getRow() && GridPane.getColumnIndex(cell) == grid.currentCell.getColumn()) {
                            if (cell instanceof Pane) {
                                ((Pane) cell).setBackground(new Background(new BackgroundFill(transparent, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                    }
                    hintCounter++;
                    finished=1;
                }
                if(!cells.contains(grid.gridCells[randomRow][randomColumn]))cells.add(grid.gridCells[randomRow][randomColumn]);
            }

            if(cells.size()<grid.getSize()*grid.getSize())grid.currentCell=grid.gridCells[randomRow][randomColumn];
            if(checkB.isSelected() && cells.size()<grid.getSize()*grid.getSize()){
                ShowMistakes(grid,gridPane);
                for (Node cell : gridPane.getChildren()) {
                    if (GridPane.getRowIndex(cell) == randomRow && GridPane.getColumnIndex(cell) == randomColumn) {
                        if (cell instanceof Pane) {
                            ((Pane) cell).setBackground(new Background(new BackgroundFill(hinted, CornerRadii.EMPTY, Insets.EMPTY)));
                        }
                    }
                }
            }
            redo.setDisable(!grid.canRedo());
            undo.setDisable(!grid.canUndo());

            //checks if won
            if (grid.checkWin()) {
                Random rand = new Random();
                for (Node nodeIn : gridPane.getChildren()) {
                    if (nodeIn instanceof Pane) {
                        float r = rand.nextFloat();
                        float g = rand.nextFloat();
                        float b = rand.nextFloat();
                        Color color = new Color(r, g, b, 0.5);
                        ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                    }
                }


                Timeline timeline = new Timeline(new KeyFrame(Duration.millis(50), event -> {
                    for (Node nodeIn : gridPane.getChildren()) {
                        if (GridPane.getRowIndex(nodeIn) == rand.nextInt(grid.getSize()) && GridPane.getColumnIndex(nodeIn) == rand.nextInt(grid.getSize())) {
                            if (nodeIn instanceof Pane) {
                                float r = rand.nextFloat();
                                float g = rand.nextFloat();
                                float b = rand.nextFloat();
                                Color color = new Color(r, g, b, 0.5);
                                ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                            }
                        }
                    }
                }));
                timeline.setCycleCount(Animation.INDEFINITE);
                timeline.play();

                /**End Prompt**/
                if(hintCounter<=grid.getSize()-2) {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setTitle("YOU WIN!!!");
                    alert.setHeaderText("CONGRATULATIONS YOU WIN!\nYou have used " + hintCounter + " hints.");
                    alert.setContentText("Press OK to go back to main menu.");
                    Optional<ButtonType> result = alert.showAndWait();
                    if (result.isPresent() && result.get() == ButtonType.OK) {
                        mainstage.close();
                    }
                    startMenu();
                }else if(hintCounter>grid.getSize()-2) {
                    Alert alert = new Alert(Alert.AlertType.INFORMATION);
                    alert.setTitle("YOU WIN!!!");
                    alert.setHeaderText("The cake is a lie!\nYou have used " + hintCounter + " hints.");
                    alert.setContentText("Press OK to go back to main menu.");
                    Optional<ButtonType> result = alert.showAndWait();
                    if (result.isPresent() && result.get() == ButtonType.OK) {
                        mainstage.close();
                    }
                    startMenu();
                }
            }
        }
    }

    // shows the solution
    class ShowSolutionButtonHandler implements EventHandler<ActionEvent> {

        Label label = null;
        Grid grid;
        Pane pane;
        GridPane gridPane;
        String number;
        int labelflag = 0;
        int finished = 0;
        Random randomCoord = new Random();
        int randomRow;
        int randomColumn;
        ArrayList<Cell> cells = new ArrayList<>();
        Color color = new Color(0, 0, 0.125, 0.25);
        Color duplicates = new Color(1, 0.5, 0.5, 1);
        Color hinted = new Color(0, 0.7, 0, 1);
        Color colorWrongSelected = new Color(0.7, 0.5, 0.5, 1);
        Color transparent = new Color(1, 1, 1, 1);
        Button redo;
        Button undo;

        public ShowSolutionButtonHandler(Grid grid, GridPane gridPane, Button undoButton, Button redoButton) {
            this.label = label;
            this.grid = grid;
            this.pane = pane;
            this.gridPane = gridPane;
            this.number = number;
            this.redo=redoButton;
            this.undo=undoButton;
        }

        @Override
        public void handle(ActionEvent actionEvent) {
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
            alert.setTitle("Solve?");
            alert.setHeaderText("Do you want to see the solution?");
            alert.setContentText(null);
            Optional<ButtonType> result = alert.showAndWait();
            if (result.isPresent() && result.get() == ButtonType.OK) {
                for (Node cell : gridPane.getChildren()) {
                    labelflag = 0;
                    if (cell instanceof Pane) {
                        ((Pane) cell).setBackground(new Background(new BackgroundFill(transparent, CornerRadii.EMPTY, Insets.EMPTY)));
                        for (Node label : ((Pane) cell).getChildren()) {
                            if (label instanceof Label) {
                                if (labelflag == 0) {
                                    grid.gridCells[GridPane.getRowIndex(cell)][GridPane.getColumnIndex(cell)].setValue(grid.randGridSolution[GridPane.getRowIndex(cell)][GridPane.getColumnIndex(cell)].getValue());
                                    ((Label) label).setText(Integer.toString(grid.randGridSolution[GridPane.getRowIndex(cell)][GridPane.getColumnIndex(cell)].getValue()));
                                    labelflag = 1;
                                    if (grid.randGridSolution[GridPane.getRowIndex(cell)][GridPane.getColumnIndex(cell)].getValue() == 0)
                                        ((Label) label).setText("");
                                }
                            }
                        }
                    }
                    // cleans the colour of the current cell so it only can set the undone cell to the selected cell
                    if (GridPane.getRowIndex(cell) == 0 && GridPane.getColumnIndex(cell) == 0) {
                        if (cell instanceof Pane) {
                            ((Pane) cell).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                        }
                    }
                    grid.currentCell = grid.gridCells[0][0];
                }
                grid.stackUndo.clear();
                grid.stackRedo.clear();
                if (grid.canRedo()) redo.setDisable(false);
                else redo.setDisable(true);
                if (grid.canUndo()) undo.setDisable(false);
                else undo.setDisable(true);

                //checks if won
                if (grid.checkWin()) {
                    Random rand = new Random();
                    for (Node nodeIn : gridPane.getChildren()) {
                        if (nodeIn instanceof Pane) {
                            float r = rand.nextFloat();
                            float g = rand.nextFloat();
                            float b = rand.nextFloat();
                            Color color = new Color(r, g, b, 0.5);
                            ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                        }
                    }


                    Timeline timeline = new Timeline(new KeyFrame(Duration.millis(50), event -> {
                        for (Node nodeIn : gridPane.getChildren()) {
                            if (GridPane.getRowIndex(nodeIn) == rand.nextInt(grid.getSize()) && GridPane.getColumnIndex(nodeIn) == rand.nextInt(grid.getSize())) {
                                if (nodeIn instanceof Pane) {
                                    float r = rand.nextFloat();
                                    float g = rand.nextFloat();
                                    float b = rand.nextFloat();
                                    Color color = new Color(r, g, b, 0.5);
                                    ((Pane) nodeIn).setBackground(new Background(new BackgroundFill(color, CornerRadii.EMPTY, Insets.EMPTY)));
                                }
                            }
                        }
                    }));
                    timeline.setCycleCount(Animation.INDEFINITE);
                    timeline.play();

                    /**End Prompt**/
                    Alert alertWin = new Alert(Alert.AlertType.INFORMATION);
                    alertWin.setTitle("You won by Cheating");
                    alertWin.setHeaderText("Congratulations? I guess...");
                    alertWin.setContentText("Press OK to go back to main menu.");
                    Optional<ButtonType> resultWin = alertWin.showAndWait();
                    if (resultWin.isPresent() && resultWin.get() == ButtonType.OK) {
                        mainstage.close();
                    }
                    startMenu();
                }
            }
        }
    }
}