import argparse
import os

parser = argparse.ArgumentParser(description='Read in a TorchScript module and print its graph to a file.')
parser.add_argument('filename', help='file to read PyTorch from')
args = parser.parse_args()
filename_head = args.filename
module = __import__(filename_head)

d = module.__dict__

jit_modules = [v for k, v in d.items() if str(type(v)) == "<class 'torch.jit.ScriptModule'>" ]

with open('tmp2.txt', 'w') as f:
    for mod in jit_modules:
        f.write(str(mod.graph).replace(os.path.abspath(module.__file__), os.path.basename(module.__file__)))
        f.write("\n")