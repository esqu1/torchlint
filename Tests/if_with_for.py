import torch

class IfWithFor(torch.nn.Module):
    def forward(self,len):
        # type: (int) -> torch.Tensor
        rv = torch.zeros(3, 4)
        for i in range(len):
            if i < 10:
                rv = rv - torch.zeros(3,4,5)
            else:
                rv = rv + 1.0
        return rv

scripted_module = torch.jit.script(IfWithFor())
