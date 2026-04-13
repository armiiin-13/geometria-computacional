# Import (plot dynamic graph)
from pyvis.network import Network
import heapq

# Declaration of colors to use (CSS style)
colors = ["red", "blue", "green", "orange", "purple", "pink", "cyan", "brown", "gray", "yellow"]

# Graph Nodes Definition (Cities of the map)
class City:
    def __init__(self, id, name):
        self.id = id
        self.name = name
        self.color = -1
        
    def __hash__(self):
        return hash(self.name)
    
    def __eq__(self, other):
        return isinstance(other, City) and self.name == other.name

    def __lt__(self, other):
        return self.id > other.id
    
class Edge:
    def __init__(self, city_1, city_2):
        self.vertex_1 = city_1
        self.vertex_2 = city_2

# Auxiliar Functions
def choose_color(adjacents):
    colors_not_used = []
    for i in range(0, len(colors)):
        colors_not_used.append(i)

    for node in adjacents:
        if node.color != -1:
            if node.color in colors_not_used:
                colors_not_used.remove(node.color)

    if colors_not_used:
        return colors_not_used[0]
    raise RuntimeError("Not enough colors defined")

def create_heap(graph):
    heap = []
    for node in graph:
        heapq.heappush(heap, (- len(graph[node]), node) )
    return heap

# Graph Coloration Algorithm
def coloration_algoritm(graph):
    heap = create_heap(graph)
    while(heap):
        _, node = heapq.heappop(heap)
        node.color = choose_color(graph[node])

    return graph

# Create Dynamic HTML to show graph
def generate_html(graph, edges):
    net = Network(directed=False)
    
    for node in graph:
        net.add_node(node.name, color=colors[node.color], title=node.name)
    
    for edge in edges:
        net.add_edge(edge.vertex_1.name, edge.vertex_2.name, color="black")
    
    net.show("coloration_graph.html", notebook=False)

# Create Data
def import_data(path):
    graph = {}
    edges = []
    cities = {}

    with open(path, "r", encoding="utf-8") as file:
        for line in file:
            name_1, name_2 = line.strip("\n").split(":") # 0 -> node 1; 1 -> node 2
        
            # Create the structures
            if name_1 not in cities:
                cities[name_1] = City(len(cities), name_1)
                graph[cities[name_1]] = []

            if name_2 not in cities:
                cities[name_2] = City(len(cities), name_2)
                graph[cities[name_2]] = []

            node_1 = cities[name_1]
            node_2 = cities[name_2]

            # Add nodes to adjacency list
            graph[node_1].append(node_2)
            graph[node_2].append(node_1)

            # Add edge to edges
            edges.append(Edge(node_1, node_2))
    return graph, edges

# Testing
test_graph, test_edges = import_data("Gotham_Map.txt")
test_graph = coloration_algoritm(test_graph)
generate_html(test_graph, test_edges)
