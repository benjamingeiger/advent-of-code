from collections import defaultdict
import json

current_day = 23
filename = f"2019-12-{current_day}.json"

board = json.load(open(filename, "r"))["members"]

# print(board)
# print(type(board))
# print(board.keys())

members = board.keys()

# for memberid in board.keys():
    # print(memberid, json.dumps(board[memberid], indent=4))

def has_star(memberid, day, star):
    try:
        val = board[memberid]["completion_day_level"][str(day)][str(star)]["get_star_ts"]
        return True
    except KeyError:
        return False

currentstate = defaultdict(int)
for memberid in members:
    for day in range(1, current_day + 1):
        for star in [1, 2]:
            if has_star(memberid, day, star):
                currentstate[(day, star)] += 1

bestscores = defaultdict(int)
for memberid in members:
    currentscore = int(board[memberid]["local_score"])
    for day in range(1, current_day + 1):
        for star in [1, 2]:
            if not has_star(memberid, day, star):
                currentscore += len(members) - currentstate[(day, star)]
    bestscores[memberid] = currentscore

# print(currentstate)

for memberid, maxscore in reversed(sorted(bestscores.items(), key=lambda x: x[1])):
    print(f"{maxscore:5}: {board[memberid]['name'] or '(anonymous user #' + str(memberid) + ')'}")
