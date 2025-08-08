import os
import re
import argparse
from chess import Board, InvalidMoveError, IllegalMoveError, AmbiguousMoveError
from chess import svg, SquareSet, BB_EMPTY, WHITE, BLACK
from prompt_toolkit import PromptSession
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.history import FileHistory
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory

script_dir = os.path.dirname(os.path.realpath(__file__))
default_svg = os.path.join(script_dir, "board.svg")
file = None
aux = []

colors = {
    "square light" : "#faf4ed",
    "square dark" : "#286983",
    "square light lastmove" : "#56949f",
    "square dark lastmove" : "#f2e9e1",
    "margin" : "#faf4ed",
    "coord" : "#286983",
    "inner border" : "#286983",
    "outer border" : "#faf4ed",
    "arrow green" : "#56949f",
    "arrow blue" : "#907aa9",
    "arrow red" : "#b4637a",
    "arrow yellow" : "#ea9d34",
}

arguments = {
    "-w": {
        "alt": "--white",
        "default": "#ffffff",
        "type": str,
        "help": "color for white box"
    },
    "-b": {
        "alt": "--black",
        "default": "#1f1f1f",
        "type": str,
        "help": "color for black box"
    },
    "-p": {
        "alt": "--pixelsize",
        "default": 1160,
        "type": int,
        "help": "size for png image on px"
    },
    "-o": {
        "alt": "--output",
        "default": default_svg,
        "type": str,
        "help": "path of png image"
    },
    "-s": {
        "alt": "--swap",
        "default": False,
        "action": "store_true",
        "help": "swap orientation of board"
    },
    "-m": {
        "alt": "--movements",
        "default": None,
        "type": str,
        "help": "movements for apply to board"
    },
    "-i": {
        "alt": "--interactive",
        "default": False,
        "action": 'store_true',
        "help": "Enter interactive movements"
    }
}

parser = argparse.ArgumentParser()
for short, params in arguments.items():

    if "action" in params:
        parser.add_argument(
            short,
            params["alt"],
            default=params["default"],
            action=params["action"],
            help=params["help"]
        )
    else:
        parser.add_argument(
            short,
            params["alt"],
            default=params["default"],
            type=params["type"],
            help=params["help"]
        )

def isEven(n):
    return n % 2 == 0

def isOdd(n):
    return not isEven(n)

def isMayus(char):
    return char == char.upper()

def drawBoard(board, args, file, fill={},
              arrows=[], squares=SquareSet(BB_EMPTY)):
    file.seek(0)
    file.truncate()
    file.write(svg.board(
        board,
        orientation= WHITE if not args.swap else BLACK,
        lastmove=board.peek(),
        size=args.pixelsize,
        fill=fill,
        arrows=arrows,
        squares=squares,
        coordinates=True,
        colors=colors
))

    
def replaceMayusMove(move):
    es_pieces = "CATDR"
    en_pieces = "NBRQK"

    if move[0] in es_pieces:
        return move.replace(move[0], en_pieces[es_pieces.index(move[0])]) 
    else:
        return move

def try_move(move, board):
    error = None
    try:
        board.push(move)
    except IllegalMoveError as e:
        print(f"Error: {e} is a illegal move.")
        error = e
    except InvalidMoveError as e:
        print(f"Error: {e} is syntactically invalid move.")
        error = e
    except AmbiguousMoveError as e:
        print(f"Error {e} is a ambiguous move.")
        error = e
        
    return error

def main():
    args = parser.parse_args()
    board = Board()
    err = False
    file = open(args.output, "w")

    if args.movements:
        aux = re.findall("[A-Za-z0-8+\\-]{2,5}", args.movements)
        for move in aux:
            replaceMayusMove(move)
            err = try_move(board.parse_san(move), board)
                
            if err:
                if not args.interactive:
                    exit(1)
                else:
                    err = False
                    break

        drawBoard(board, args, file)

    if args.interactive:
        bindings = KeyBindings()
        session = PromptSession(
            key_bindings=bindings,
            history=FileHistory("./history"),
            auto_suggest=AutoSuggestFromHistory(),
        )

        @bindings.add('c-q')
        def _(event):
            args.interactive = False
            event.app.exit()

        @bindings.add('c-p')
        def _(event):
            global aux
            aux.append(board.pop())
            drawBoard(board, args, file)
            event.app.exit()

        @bindings.add('c-n')
        def _(event):
            global aux
            if len(aux) > 0:
                e = try_move(aux.pop(), board)
                drawBoard(board, args, file)

            event.app.exit()

        @bindings.add('c-e')
        def _(event):
            print(board.board_fen())
            event.app.exit()

        @bindings.add('c-g')
        def _(event):
            pgn = ""
            for move in board.move_stack:
                if isEven(board.move_stack.index(move)):
                    pgn += f"{board.move_stack.index(move) // 2 + 1}. {move} "
                else:
                    pgn += f"{move} "
            print(pgn)
            event.app.exit()

        @bindings.add('c-d')
        def _(event):
            if len(board.move_stack) > 0:
                board.pop()
                drawBoard(board, args, file)

            event.app.exit()

        @bindings.add('c-r')
        def _(event):
            board.reset()
            drawBoard(board, args, file)
            event.app.exit()

        @bindings.add('c-f')
        def _(event):
            args.swap = not args.swap
            drawBoard(board, args, file)
            event.app.exit()
            
        while args.interactive:
            input = session.prompt('>>> ')

            if input:
                err = try_move(board.parse_san(replaceMayusMove(input)), board)
                drawBoard(board, args, file)

    file.close()
    exit(0)
    
if __name__ == '__main__':
    main()
