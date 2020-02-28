import torch

class GoodDevice1Test(torch.nn.Module):
    def forward(self):
        tensor1 = torch.zeros(3,4).cpu()
        tensor2 = torch.zeros(3,4).to(device='cpu')
        return torch.dot(tensor1, tensor2)
        
scripted_module = torch.jit.script(GoodDevice1Test())