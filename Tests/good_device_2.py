import torch
import torch.nn.functional as F

class GoodDevice2Test(torch.nn.Module):
  def forward(self):
      x = torch.ones(3,1).cuda()
      y = torch.zeros(3,1).cuda()
      return x + y

scripted_module = torch.jit.script(GoodDevice2Test())
