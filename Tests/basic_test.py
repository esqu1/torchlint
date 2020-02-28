import torch
import torch.nn.functional as F

class BasicTest(torch.nn.Module):
  def forward(self):
    x = torch.ones(3,1).cuda()
    return x

scripted_module = torch.jit.script(BasicTest())
