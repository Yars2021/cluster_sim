import ast
import functools
import sys

from os import listdir
from os.path import isfile, join


def read_strat_file(filepath):
    def sort_comparator(a, b):
        [i_a, h_a, _, _] = a
        [i_b, h_b, _, _] = b

        if i_a < i_b:
            return -1
        elif i_a == i_b:
            if h_a < h_b:
                return 1
            elif i_a == i_b:
                return 0
            else:
                return -1
        else:
            return 1

    term = ""

    with open(filepath, "r") as file:
        for line in file:
            term += line

    return sorted(ast.literal_eval(term), key=functools.cmp_to_key(sort_comparator))


strat_tables = {}
sent_stats = {}


for file in [file for file in listdir(sys.argv[1]) if isfile(join(sys.argv[1], file))]:
    if file[-5:] == ".info":
        sent_stat = ""

        with open(f"{sys.argv[1]}/{file[:-5]}.info.sent", "r") as sent_file:
            for line in sent_file:
                sent_stat += line

        strat_tables[file[:-5]] = {}
        sent_stats[file[:-5]] = ast.literal_eval(sent_stat)

        for [node, ttl, status, route] in read_strat_file(f"{sys.argv[1]}/{file}"):
            if node not in strat_tables[file[:-5]]:
                strat_tables[file[:-5]][node] = [[status, ttl, route]]
            else:
                strat_tables[file[:-5]][node].append([status, ttl, route])


strat_tables_html = ""


def packet_sort_comparator(a, b):
    [ttl_a, _, _] = a
    [ttl_b, _, _] = b

    if ttl_a < ttl_b:
        return -1
    elif ttl_a == ttl_b:
        return 0
    else:
        return 1


for strat in strat_tables:
    strat_tables_html += f"<hr><h3>Strategy: {strat} ({(len(strat_tables[strat]) / int(sys.argv[3]) * 100):.{3}f}%)</h3><hr><table class=\"tel_table\">"
    strat_tables_html += f"<tr><th>Node ID</th><th>Hops of the packet</th><th>Route of the packet</th><th>Transmit status</th></tr>"

    for node in strat_tables[strat]:
        sorted_nodes = sorted(strat_tables[strat][node], key=functools.cmp_to_key(packet_sort_comparator))

        for [status, ttl, route] in sorted_nodes:
            ping_info = f""
            route_str = ""

            for route_node in route:
                if route_node == 0:
                    route_str += ""
                else:
                    route_str += str(route_node) + " => "

            ping_info += f"<td>{int(sys.argv[4]) - ttl} of {int(sys.argv[4])}</td><td>{route_str[:-4]}</td><td>{status}</td>"
            strat_tables_html += f"<tr><td>{node}</td>{ping_info}</tr>"

    strat_tables_html += f"</table><div class=\"small_table\"><table class=\"tel_table_small\"><tr><th>Node ID</th><th>Packets sent by node</th></tr>"

    sum = 0

    for i in range(len(sent_stats[strat])):
        strat_tables_html += f"<tr><td>{i + 1}</td><td>{sent_stats[strat][i][1]}</td></tr>"
        sum += sent_stats[strat][i][1]

    strat_tables_html += f"<tr><th>SUM</th><th>{sum}</th></tr></table></div>"


css = ""

with open(f"./data_processor/index.css", "r") as file:
    for line in file:
        css += line


svg_data = ""

with open(f"{sys.argv[2]}/cluster.svg", "r") as file:
    for line in file:
        svg_data += line


html = f"""
<html>
    <head>
        <title>Cluster sim results</title>
        <style>
            {css}
        </style>
    </head>
    <body>
        <div class="info_header">
            <h3>Number of nodes in cluster: {sys.argv[3]}</h3>
            <h3>Packet TTL parameter: {sys.argv[4]}</h3>
            <hr>
        </div>
        <div class="cluster_image">
            {svg_data}
        </div>
        <div class="cluster_data">
            <hr>
            <h3>Info about first received packets and packets to be deleted by TTL reaching 0</h3>
            {strat_tables_html}
        </div>
    </body>
</html>
"""


with open(f"{sys.argv[2]}/index.html", "w") as file:
    print(html, file=file)
