import torch
import torch.nn.functional as F

class ForTest(torch.nn.Module):
    def forward(self):
        x = torch.zeros(3,1)
        for t in range(10):
            x = x + 1
        return x

scripted_module = torch.jit.script(ForTest())