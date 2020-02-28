import torch

class BadWhileDevice(torch.nn.Module):
    def forward(self):
        device = torch.device('cuda:0')
        tensor1 = torch.zeros(3,4).cuda().float()
        tensor2 = torch.zeros(3,4).cuda()

        while 1 > 0:
            tensor2 = tensor1 + tensor2
        tensor4 = tensor2 + torch.ones(4,5) # should error 
        return tensor4
        
scripted_module = torch.jit.script(BadWhileDevice())