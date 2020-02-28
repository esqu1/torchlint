import torch

class BadIfDevice2(torch.nn.Module):
    def forward(self):
        tensor1 = torch.zeros(3,4).cuda()
        tensor2 = torch.zeros(3,4).cuda()

        if tensor1 > 0:
            tensor3 = tensor1 + tensor2
        else:
            tensor3 = torch.ones(3,4).cuda()
        tensor4 = tensor3 + torch.ones(4,5) # should error 
        return tensor4
        
scripted_module = torch.jit.script(BadIfDevice2())