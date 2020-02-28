import torch

class BadDevice1Test(torch.nn.Module):
    def forward(self):
        device = torch.device('cuda:0')
        tensor1 = torch.zeros(3,4).cpu()
        tensor2 = torch.zeros(3,4).to(device)
        return tensor1 + tensor2
        
scripted_module = torch.jit.script(BadDevice1Test())