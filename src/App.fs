module App

module SnakeGame =
    open System

    type Direction =
        | Right
        | Left
        | Up
        | Down

    type Position = { 
        X : int
        Y : int 
    }

    type Snake = {
        Direction : Direction
        Length : int
        Trail : Position list
    }

    type Game = {
        Snake : Snake
        Apple : Position
        GridSize : int
        Score : int
    }

    type GameState =
        | Alive of Snake
        | Score of Snake
        | Dead

    let defaultHead = { X = 10 ; Y = 10 }
    let defaultGridSize = 20
    let getHead snake = 
        snake.Trail.[snake.Trail.Length - 1]

    let getApple() =
        let randomizer = Random()
        { 
            X = randomizer.Next(0, defaultGridSize)
            Y = randomizer.Next(0, defaultGridSize)
        }

    let checkColisionBetween positionA positionB =
        positionA.X = positionB.X 
        && positionA.Y = positionB.Y

    let checkOutOfBounds newX newY =
        match (newX, newY) with
        | (x,y) when x < 0 -> defaultGridSize-1, y
        | (x,y) when y < 0 -> x, defaultGridSize-1
        | (x,y) when x > defaultGridSize-1 -> 0, y
        | (x,y) when y > defaultGridSize-1 -> x, 0
        | (x,y) -> (x,y)

    let getNextPosition snake =
        let (changeX, changeY) = 
            match snake.Direction with
            | Direction.Right -> (1,  0)
            | Direction.Left ->  (-1, 0)
            | Direction.Up ->    (0, -1)
            | Direction.Down ->  (0,  1) 

        let head = getHead snake
        let (newX, newY) = checkOutOfBounds (head.X + changeX) (head.Y + changeY)

        { X = newX ; Y = newY }

    let move snake toPosition =
        let skipSize = Math.Max(0, snake.Trail.Length + 1 - snake.Length)
        { snake with 
            Trail = snake.Trail @ [toPosition]
                    |> List.skip skipSize    
        }

    let checkColisions apple snake  =
        let rec checkBodyColision head trailPositions =
            match trailPositions with
            | [] -> Alive snake
            | current :: [] -> Alive snake
            | current :: tail when (checkColisionBetween head current) -> Dead
            | current :: tail -> checkBodyColision head tail

        let head = getHead snake
        if checkColisionBetween head apple
            then Score snake
            else checkBodyColision head snake.Trail

    let continueGame game snake direction =
        {game with Snake = {snake with Direction = direction}}

    let score game snake direction =
        {game with 
                Snake = {snake with Direction = direction ; Length = snake.Length + 1}
                Score = game.Score + 1
                Apple = getApple()
        }

    let run game =
        game.Snake
        |> getNextPosition
        |> move game.Snake
        |> checkColisions game.Apple


open Fable.Core.JsInterop
open Fable.Import
open Browser.Types
open SnakeGame

let window = Browser.Dom.window
let document = Browser.Dom.document

let mutable myCanvas : Browser.Types.HTMLCanvasElement = 
    unbox window.document.getElementById "myCanvas"

let context = myCanvas.getContext_2d()

let defaultTileSize = myCanvas.width / (defaultGridSize |> float)
let mutable direction = Direction.Right
let defaultGameSettings = {
    Apple = getApple()
    Score = 0
    GridSize = defaultGridSize
    Snake = {
        Trail = [ defaultHead ]
        Direction = Direction.Right
        Length = 5
    }
}

let isValidChange fromDirection toDirection =
    fromDirection = Direction.Right && toDirection <> Direction.Left
    || fromDirection = Direction.Left && toDirection <> Direction.Right
    || fromDirection = Direction.Up && toDirection <> Direction.Down
    || fromDirection = Direction.Down && toDirection <> Direction.Up

let commandPressed (event:KeyboardEvent)= 
    let newDirection = 
        match event.keyCode with
        | 37.0 -> Direction.Left
        | 38.0 -> Direction.Up
        | 39.0 -> Direction.Right
        | 40.0 -> Direction.Down
        | _ -> direction

    if isValidChange direction newDirection
        then direction <- newDirection
    ()


let getCanvasPosition position =
    position
    |> float
    |> (*) defaultTileSize

let printCanvas game = 
    context.fillStyle <- !^ "black"
    context.fillRect (0., 0., myCanvas.width, myCanvas.height)

    context.fillStyle <- !^ "lime"
    for position in game.Snake.Trail do
        context.fillRect (  position.X |> getCanvasPosition, 
                            position.Y |> getCanvasPosition, 
                            defaultTileSize - 2., defaultTileSize - 2.)

    context.fillStyle <- !^ "red"
    context.fillRect (  game.Apple.X |> getCanvasPosition, 
                        game.Apple.Y |> getCanvasPosition, 
                        defaultTileSize - 2., defaultTileSize - 2.)

let resetGame score =
    window.alert(sprintf "Score: %i" score) 
    direction <- Direction.Right
    defaultGameSettings

let rec snakeGame game = 
    printCanvas game
    let state = run game
    let updatedGame = 
        match state with
        | Alive snake -> continueGame game snake direction
        | Score snake -> score game snake direction
        | Dead -> resetGame game.Score
    
    window.setTimeout( (fun args -> snakeGame updatedGame), 1000/15) |> ignore
    
document.addEventListener("keydown", fun event -> commandPressed(event :?> _))
snakeGame defaultGameSettings |> ignore







