import os

dirname = os.getcwd()

(_, _, filenames) = next(os.walk('.'))

for filename in filenames:
    filename_head = filename.split('.')[0]
    if (filename_head != 'print_graphs'):

        module = __import__(filename_head)

        d = module.__dict__

        jit_modules = [v for k, v in d.items() if str(type(v)) == "<class 'torch.jit.ScriptModule'>" ]

        print(jit_modules)

        with open(os.path.join('Python_Graphs',filename_head+'.txt'), 'w') as f:
            for mod in jit_modules:
                f.write(str(mod.graph).replace(os.path.abspath(module.__file__), os.path.basename(module.__file__)))
                f.write("\n")