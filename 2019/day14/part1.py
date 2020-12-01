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

quantities = defaultdict(int)
quantities["FUEL"] = 1

for product in processorder:
    if product == "ORE":
        print("needed:", quantities["ORE"])
    elif product in quantities.keys():
        productquantity = quantities[product]
        del quantities[product]

        resultquantity, reagents = reactions[product]
        numreactions = 0
        while productquantity > 0:
            numreactions += 1
            productquantity -= resultquantity

        for reagent, reagentquantity in reagents:
            quantities[reagent] += reagentquantity * numreactions
