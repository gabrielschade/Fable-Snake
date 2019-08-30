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
        Trail : Position list
        Tail : int
        Direction : Direction
        Head : Position
    }

    type Game = {
        Snake : Snake
        Apple : Position
        GridSize : int
        Score : int
    }

    type GameState =
        | Alive of Snake
        | NewScore of Snake
        | Dead

    let defaultHead = { X = 10 ; Y = 10 }
    let defaultGridSize = 20

    let getApple() =
        let randomizer = Random()
        { 
            X = randomizer.Next(0, 20)
            Y = randomizer.Next(0, 20)
        }

    let checkOutOfBounds newX newY =
        match (newX, newY ) with
        | (x,y) when x < 0 -> defaultGridSize-1, y
        | (x,y) when y < 0 -> x, defaultGridSize-1
        | (x,y) when x > defaultGridSize-1 -> 0, y
        | (x,y) when y > defaultGridSize-1 -> x, 0
        | (x,y) -> (x,y)

    let getNextPosition snake =
        let (changeX, changeY) = 
            match snake.Direction with
            | Direction.Right -> (1, 0)
            | Direction.Left -> (-1,0)
            | Direction.Up -> (0, -1)
            | Direction.Down -> (0, 1) 

        let (newX, newY) = 
            checkOutOfBounds (snake.Head.X + changeX) (snake.Head.Y + changeY)

        { snake with Head = { X = newX ; Y = newY }}

    let move snake =
        let skipSize = Math.Max(0, snake.Trail.Length + 1 - snake.Tail)
        let trail = 
            snake.Trail 
            @ [snake.Head]
            |> List.skip skipSize
        { snake with Trail = trail }

    let checkColisions apple snake  =
        let rec checkBodyColision head trailPositions =
            match trailPositions with
            | current :: [] -> Alive snake
            | current :: tail when head.X = current.X && head.Y = current.Y -> Dead
            | current :: tail -> checkBodyColision head tail
            | [] -> Alive snake

        let head = snake.Head
        if head.X = apple.X && head.Y = apple.Y
            then NewScore snake
            else checkBodyColision snake.Head snake.Trail

    let run game =
        game.Snake
        |> getNextPosition
        |> move
        |> checkColisions game.Apple


open Fable.Core.JsInterop
open Fable.Import
open Browser.Types
open SnakeGame

let window = Browser.Dom.window
let document = Browser.Dom.document

let mutable myCanvas : Browser.Types.HTMLCanvasElement = 
    unbox window.document.getElementById "myCanvas"

let mutable direction = Direction.Right
let context = myCanvas.getContext_2d()
let commandPressed (event:KeyboardEvent)= 
    direction <- 
        match event.keyCode with
        | 37.0 -> Direction.Left
        | 38.0 -> Direction.Up
        | 39.0 -> Direction.Right
        | 40.0 -> Direction.Down
        | _ -> direction
    ()
document.addEventListener("keydown", fun event -> commandPressed(event :?> _))

let defaultGameSettings = {
    Apple = getApple()
    Score = 0
    GridSize = defaultGridSize
    Snake = {
        Head = defaultHead
        Trail = [ defaultHead ]
        Direction = Direction.Right
        Tail = 5
    }
}

let printCanvas game = 
    context.fillStyle <- !^ "black"
    context.fillRect (0., 0., myCanvas.width, myCanvas.height)

    context.fillStyle <- !^ "lime"
    for position in game.Snake.Trail do
        context.fillRect (  position.X * game.GridSize |> float, 
                            position.Y * game.GridSize |> float, 
                            18., 18.)

    context.fillStyle <- !^ "red"
    context.fillRect (  game.Apple.X * game.GridSize |> float, 
                        game.Apple.Y * game.GridSize |> float, 
                        18., 18.)

let resetGame score =
    window.alert(sprintf "Score: %i" score) 
    direction <- Direction.Right
    defaultGameSettings

let rec snakeGame game = 
    printCanvas game
    let state = run game
    let updatedGame = 
        match state with
        | Alive snake -> {game with Snake = {snake with Direction = direction}}
        | NewScore snake -> {game with 
                                Snake = {snake with Direction = direction ; Tail = snake.Tail + 1}
                                Score = game.Score + 1
                                Apple = getApple()
                            }
        | Dead -> resetGame game.Score

    window.setTimeout( (fun args -> snakeGame updatedGame), 1000/15) |> ignore

snakeGame defaultGameSettings |> ignore







