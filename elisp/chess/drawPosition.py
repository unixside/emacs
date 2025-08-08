import os
import re
import argparse
from chess import Board, InvalidMoveError, IllegalMoveError, AmbiguousMoveError
from chess import svg, SquareSet, BB_EMPTY, WHITE, BLACK, SAN_REGEX
from prompt_toolkit import PromptSession
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.history import FileHistory
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory

script_dir = os.path.dirname(os.path.realpath(__file__))
default_svg = os.path.join(script_dir, "board.svg")
file = None
aux = []
es_pieces = "CATDR"
en_pieces = "NBRQK"

CASTLING_REGEX = re.compile(r"^(0-0)(-0)?([+#]{1})?\Z")

colors = {
    "square light" : "#faf4ed",
    "square dark" : "#286983",
    "square light lastmove" : "#56949f",
    "square dark lastmove" : "#56949f",
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
    },
    "-c":
    {
        "alt": "--coordinates",
        "default": False,
        "action": "store_true",
        "help": "Enables drawing of coordinates on the board edges."
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

def is_even(n):
    return n % 2 == 0

def is_odd(n):
    return not is_even(n)

def is_mayus(char):
    return char == char.upper()

def draw_board(board, args, file, fill={},
              arrows=[], squares=SquareSet(BB_EMPTY)):
    file.seek(0)
    file.truncate()
    file.write(svg.board(
        board,
        orientation= WHITE if not args.swap else BLACK,
        lastmove=board.peek() if len(board.move_stack) > 0 else None,
        size=args.pixelsize,
        fill=fill,
        arrows=arrows,
        squares=squares,
        coordinates=args.coordinates,
        colors=colors
))

def san_match(text):
    return SAN_REGEX.match(text) or CASTLING_REGEX.match(text)

def replace_piece_char_of_move(move):
    _aux = move
    if move[0] in es_pieces:
        _aux = move.replace(move[0], en_pieces[es_pieces.index(move[0])])
    
    return _aux

def parse_movements(input):
    substr = map(replace_piece_char_of_move, input.split(" "))
    matchs = list(map(san_match, substr))
    movements = [ s.group(0) for s in matchs if s is not None ]

    return movements

def try_move(san, board):
    error = False
    try:
        move = board.parse_san(san)
        board.push(move)
    except IllegalMoveError:
        print(f"Error: {san} is a illegal move.")
        error = True
    except InvalidMoveError:
        print(f"Error: {san} is syntactically invalid move.")
        error = True
    except AmbiguousMoveError:
        print(f"Error {san} is a ambiguous move.")
        error = True
        
    return error

def load_movements(input, board, interactive):
    error = False
    index = 0
    movements = parse_movements(input)
    while not error and index < len(movements):
        error = try_move(movements[index], board)
        index += 1
    else:
        if not interactive:
            exit(1)                
    return error

def main():
    args = parser.parse_args()
    board = Board()
    file = open(args.output, "w")

    if args.movements:
       load_movements(args.movements, board, args.interactive)

    draw_board(board, args, file)

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
            if len(board.move_stack) > 0:
                aux.append(board.pop())
                draw_board(board, args, file)
            event.app.exit()

        @bindings.add('c-n')
        def _(event):
            global aux
            if len(aux) > 0:
                board.push(aux.pop())
                draw_board(board, args, file)
                
            event.app.exit()

        @bindings.add('c-e')
        def _(event):
            print(board.board_fen())
            event.app.exit()

        @bindings.add('c-g')
        def _(event):
            line=""
            _aux = Board()
            for move in board.move_stack:
                index = board.move_stack.index(move)
                if is_even(index):
                    line += f"{index // 2 + 1}. {_aux.san_and_push(move)} "
                else: 
                    line += f"{_aux.san_and_push(move)} "

            print(line)
            event.app.exit()

        @bindings.add('c-d')
        def _(event):
            if len(board.move_stack) > 0:
                board.pop()
                aux.clear()                    
                draw_board(board, args, file)

            event.app.exit()

        @bindings.add('c-r')
        def _(event):
            board.reset()
            aux.clear()
            draw_board(board, args, file)
            event.app.exit()

        @bindings.add('c-f')
        def _(event):
            args.swap = not args.swap
            draw_board(board, args, file)
            event.app.exit()
            
        while args.interactive:
            input = session.prompt('>>> ')
            if input:
                load_movements(input, board, args.interactive)
                draw_board(board, args, file)

    file.close()
    exit(0)
    
if __name__ == '__main__':
    main()
