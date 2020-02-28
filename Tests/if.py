import torch

class IfTest(torch.nn.Module):
  def forward(self):
    x = torch.ones(3,1)
    y = 3*torch.ones(3,1)
    z = torch.zeros(3,1)
    if (x > 0).all():
      k = x + y
    else:
      k = x + z
    return k

scripted_module = torch.jit.script(IfTest())
