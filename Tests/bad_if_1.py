import torch
import torch.nn.functional as F

class BadIf1Test(torch.nn.Module):
  def forward(self):
    x = torch.ones(3,1)
    y = 3*torch.ones(5,1)
    z = torch.zeros(3,1)
    if (x > 0).all():
      k = x + y
    else:
      k = x + z
    return k

scripted_module = torch.jit.script(BadIf1Test())
