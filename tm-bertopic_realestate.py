# pip install ipywidgets
# pip3 install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cu118
import torch
torch.cuda.is_available()
torch.__version__
from torch import nn
import torch.nn.functional as F
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader

import numpy as np
# pip install mxnet -f https://dist.mxnet.io/python/cpu
import gluonnlp as nlp
from gluonnlp.data import SentencepieceTokenizer
from kobert.utils import get_tokenizer
from tqdm.notebook import tqdm

from kobert import get_tokenizer
from kobert import get_pytorch_kobert_model