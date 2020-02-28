import torch
import torch.nn.functional as F

class BadDevice2Test(torch.nn.Module):
  def forward(self):
    x = torch.zeros(3,1).cpu()
    y = torch.zeros(3,1).cuda()
    return x + y

scripted_module = torch.jit.script(BadDevice2Test())
