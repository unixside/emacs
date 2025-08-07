import os
import re
import argparse
from chess import Board, InvalidMoveError, IllegalMoveError, AmbiguousMoveError
from PIL import Image, ImageDraw
from prompt_toolkit import PromptSession
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.history import FileHistory
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory

board_png = "./board.png"
pieces = {
    "P" : "img/white-pawn.png",
    "N" : "img/white-knight.png",
    "B" : "img/white-bishop.png",
    "R" : "img/white-rook.png",
    "Q" : "img/white-queen.png",
    "K" : "img/white-king.png",
    "p" : "img/black-pawn.png",
    "n" : "img/black-knight.png",
    "b" : "img/black-bishop.png",
    "r" : "img/black-rook.png",
    "q" : "img/black-queen.png",
    "k" : "img/black-king.png"
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
        "default": "./board.png",
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

script_dir = os.path.dirname(os.path.realpath(__file__))

for key, path in pieces.items():
    pieces[key] = os.path.join(script_dir, path)


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

def isWhiteBox(row, col):
    return (isEven(row) and isEven(col)) or (isOdd(row) and isOdd(col))

def reverseStr(s):
    return s[::-1]

def getPiece(c, s):
    return Image.open(pieces.get(c), "r").convert("RGBA").copy().resize((s, s))

def drawBoard(fen, swap_orientation, size, wcolor, bcolor, output):
    img = Image.new("RGBA", (size, size))
    row = 0
    col = 0
    x = 0
    y = 0
    s = size//8

    if swap_orientation:
        pfen = prossecedFen(reverseStr(fen))
    else:
        pfen = prossecedFen(fen)

    for c in pfen:
        x = col * s
        y = row * s
        color = wcolor if isWhiteBox(row, col) else bcolor

        img1 = ImageDraw.Draw(img)
        img1.rectangle([x,y,x + s, y + s], fill=color)

        if c in pieces.keys():
            piece = getPiece(c, s)
            img.paste(piece, (x,y), piece)

        if (col == 7):
            col = 0
            row += 1
        else:
            col += 1

    img.save(output, format="png")

def drawMyBoard(board, args):
    drawBoard(
        board.board_fen(),
        args.swap,
        args.pixelsize,
        args.white,
        args.black,
        args.output,
    )

def prossecedFen(fen):
    pfen = ""
    def fun(c): return "0" * int(c) if c.isnumeric() else c
    
    for c in fen.replace("/", ""):
        pfen = pfen + fun(c)
        
    return pfen

def replaceMayusPieces(move):
    es_pieces = "CATDR"
    en_pieces = "NBRQK"

    if move[0] in es_pieces:
        return move.replace(move[0], en_pieces[es_pieces.index(move[0])]) 
    else:
        return move

def try_move(move, board):
    error = None
    if isMayus(move[0]):
        move = replaceMayusPieces(move)
    try:
        board.push_san(move)
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
    movements = args.movements if args.movements else []
    aux = []
    err = False

    if movements != []:
        movements = re.findall("[A-Za-z0-8+\\-]{2,5}", movements)
        for move in movements:
            err = try_move(move, board)
                
            if err:
                if not args.interactive:
                    exit(1)
                else:                
                    movements = movements[0, movements.index(move)]
                    err = False                

        drawMyBoard(board, args)

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
            if len(movements) > 0:
                aux.append(movements.pop())
                board.pop()
                drawMyBoard(board, args)

            event.app.exit()

        @bindings.add('c-n')
        def _(event):
            if len(aux) > 0:
                move = aux.pop()
                movements.append(move)
                board.push_san(move)
                drawMyBoard(board, args)

            event.app.exit()

        @bindings.add('c-e')
        def _(event):
            print(board.board_fen())
            event.app.exit()

        @bindings.add('c-g')
        def _(event):
            pgn = ""
            for move in movements:
                if isEven(movements.index(move)):
                    pgn += f"{movements.index(move) // 2 + 1}. {move} "
                else:
                    pgn += f"{move} "
            print(pgn)
            event.app.exit()

        @bindings.add('c-d')
        def _(event):
            board.pop()
            drawMyBoard(board, args)
            event.app.exit()

        @bindings.add('c-r')
        def _(event):
            board.reset()
            movements.clear()
            args.swap = False
            drawMyBoard(board, args)
            event.app.exit()
            
        while args.interactive:            
            input = session.prompt('>>> ')

            if input:
                err = try_move(input, board)
                movements.append(input)
                drawMyBoard(board, args)
                
    exit(0)
    
if __name__ == '__main__':
    main()
