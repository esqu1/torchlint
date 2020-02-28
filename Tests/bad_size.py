import torch

class BadSize(torch.nn.Module):
    def forward(self):
        tensor1 = torch.zeros(5,2)
        tensor2 = torch.zeros(3,7)
        return tensor1 + tensor2
        
scripted_module = torch.jit.script(BadSize())