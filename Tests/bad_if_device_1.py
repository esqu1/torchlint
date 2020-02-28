import torch

class BadIfDevice1(torch.nn.Module):
    def forward(self):
        device = torch.device('cuda:0')
        tensor1 = torch.zeros(3,4).cuda()
        tensor2 = torch.zeros(3,4).cuda()

        if tensor1 > 0:
            tensor3 = tensor1 + tensor2
        else:
            tensor3 = torch.ones(3,4)
        return tensor3
        
scripted_module = torch.jit.script(BadIfDevice1())