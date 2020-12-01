import networkx as nx
from networkx.algorithms.dag import topological_sort
from collections import defaultdict

reactions = {}
for line in open("input.txt", "r"):
    reagents, product = line.split("=>")

    reagents = [x.strip() for x in reagents.split(",")]
    reagentlist = []
    for reagent in reagents:
        q, r = reagent.split()
        reagentlist.append((r, int(q)))

    quantity, product = product.strip().split(" ")
    reactions[product.strip()] = (int(quantity), reagentlist)

G = nx.DiGraph()

G.add_node("ORE")
for reagent in reactions.keys():
    G.add_node(reagent)

for product, (quantity, reagents) in reactions.items():
    for (reagent, quantity_) in reagents:
        G.add_edge(product, reagent)

processorder = list(topological_sort(G))


for target in range(6200000, 6300000):
# for target in [6200000]:
    quantities = defaultdict(int)
    quantities["FUEL"] = target
    print(target)
    for product in processorder:
        if product == "ORE":
            print("needed:", quantities["ORE"])
            if quantities["ORE"] > 1000000000000:
                print("make less")
            elif quantities["ORE"] < 1000000000000:
                print("make more")
        elif product in quantities.keys():
            productquantity = quantities[product]
            del quantities[product]

            resultquantity, reagents = reactions[product]
            numreactions = productquantity // resultquantity
            if productquantity % resultquantity > 0:
                numreactions += 1

            for reagent, reagentquantity in reagents:
                quantities[reagent] += reagentquantity * numreactions
