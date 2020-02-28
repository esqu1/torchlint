import torch

class BadDeviceMult(torch.nn.Module):
    def forward(self):
        device = torch.device('cuda:0')
        tensor1 = torch.zeros(3,4).cuda().float()
        tensor2 = torch.zeros(3,4)
        tensor3 = torch.zeros(3,4).cuda()
        tensor4 = torch.zeros(3,4)
        if 1 > 0:
            tensor3 = tensor1 + torch.zeros(3,4).cuda()
            tensor4 = tensor2
        return tensor3 + tensor4
        
scripted_module = torch.jit.script(BadDeviceMult())

print(scripted_module.graph)