import torch
import torch.nn.functional as F

class BadIf2Test(torch.nn.Module):
  def forward(self):
    x = torch.ones(3,1)
    y = 3*torch.ones(3,1)
    z = torch.zeros(5,1)
    if (x > 0).all():
      k = x + y
    else:
      k = x + z
    return k

scripted_module = torch.jit.script(BadIf2Test())
