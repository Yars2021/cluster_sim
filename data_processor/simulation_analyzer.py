import ast
import functools
import sys

from os import listdir
from os.path import isfile, join


def read_strat_file(filepath):
    def sort_comparator(a, b):
        [i_a, _, h_a, _] = a
        [i_b, _, h_b, _] = b

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


for file in [file for file in listdir(sys.argv[1]) if isfile(join(sys.argv[1], file))]:
    if file[-5:] == ".info":
        strat_tables[file[:-5]] = {}

        for [node, src, ttl, status] in read_strat_file(f"{sys.argv[1]}/{file}"):
            if node not in strat_tables[file[:-5]]:
                strat_tables[file[:-5]][node] = [status, [[src, ttl]]]
            else:
                strat_tables[file[:-5]][node][1].append([src, ttl])


strat_tables_html = ""


def packet_sort_comparator(a, b):
    [_, ttl_a] = a
    [_, ttl_b] = b

    if ttl_a < ttl_b:
        return -1
    elif i_a == i_b:
        return 0
    else:
        return 1


for strat in strat_tables:
    strat_tables_html += f"<hr><h3>Strategy: {strat}</h3><hr><table class=\"tel_table\">"
    strat_tables_html += f"<tr><th>Node ID</th><th>Transmit status</th><th>First received ping info</th></tr>"

    for node in strat_tables[strat]:
        sorted_srcs = sorted(strat_tables[strat][node][1], key=functools.cmp_to_key(packet_sort_comparator))

        ping_info = f"<table class=\"ping_table\"><tr><th>Source</th><th>Hops it took to reach</th></tr>"

        for [src, ttl] in sorted_srcs:
            if src == 0:
                src_str = "Monitor"
            else:
                src_str = str(src)

            ping_info += f"<tr><td>{src_str}</td><td>{int(sys.argv[4]) - ttl}</td></tr>"

        ping_info += f"</table>"

        strat_tables_html += f"<tr><td>{node}</td><td>{strat_tables[strat][node][0]}</td><td class=\"nested_table\">{ping_info}</td></tr>"

    strat_tables_html += "</table>"


svg_data = ""


with open(f"{sys.argv[2]}/cluster.svg", "r") as file:
    for line in file:
        svg_data += line


html = f"""
<html>
    <head>
        <title>Cluster sim results</title>
        <link href="../data_processor/index.css" rel="stylesheet">
    </head>
    <body>
        <div class="info_header">
            <h3>Number of nodes in cluster: {sys.argv[3]}</h3>
            <h3>Packet TTL parameter: {sys.argv[4]}</h3>
        </div>
        <div class="cluster_image">
            {svg_data}
        </div>
        <div class="cluster_data">
            {strat_tables_html}
        </div>
    </body>
</html>
"""


with open(f"{sys.argv[2]}/index.html", "w") as file:
    print(html, file=file)
