# 3단계: 모델 정의
import torch
import torch.nn as nn
import torchaudio
from torch.utils.data import DataLoader, Dataset
import torchaudio.transforms as T

class VoiceConversionModel(nn.Module):
    def __init__(self):
        super(VoiceConversionModel, self).__init__()
        # 모델 아키텍처 정의, 입력 채널을 2로 설정
        self.conv1 = nn.Conv1d(in_channels=2, out_channels=16, kernel_size=3, stride=1, padding=1)
        self.relu = nn.ReLU()
        self.conv2 = nn.Conv1d(in_channels=16, out_channels=2, kernel_size=3, stride=1, padding=1)

    def forward(self, x):
        x = self.relu(self.conv1(x))
        x = self.conv2(x)
        return x

# 4단계: 데이터 로딩 및 전처리
class VoiceDataset(Dataset):
    def __init__(self, source_paths, target_paths):
        super(VoiceDataset, self).__init__()
        self.source_paths = [source_paths]  # 리스트로 감싸기
        self.target_paths = [target_paths]  # 리스트로 감싸기

    @staticmethod
    def resample_waveform(waveform, source_sr, target_sr):
        if source_sr != target_sr:
            resampler = T.Resample(source_sr, target_sr)
            waveform = resampler(waveform)
        return waveform

    def __len__(self):
        return len(self.source_paths)

    def __getitem__(self, idx):
        source_path = self.source_paths[idx]
        target_path = self.target_paths[idx]
        source_waveform, source_sr = torchaudio.load(source_path)
        target_waveform, target_sr = torchaudio.load(target_path)
        # 가정: 모든 오디오를 16000Hz로 재샘플링
        source_waveform = self.resample_waveform(source_waveform, source_sr, 16000)
        target_waveform = self.resample_waveform(target_waveform, target_sr, 16000)
        # 길이 맞추기 (가장 짧은 길이에 맞춤)
        min_len = min(source_waveform.size(1), target_waveform.size(1))
        source_waveform = source_waveform[:, :min_len]
        target_waveform = target_waveform[:, :min_len]
        return source_waveform, target_waveform



# 예시 데이터셋 경로 리스트
source_paths = "D:/대학원/librosa/source_voice.mp3"
target_paths = "D:/대학원/librosa/target_song.mp3"

dataset = VoiceDataset(source_paths, target_paths)
dataloader = DataLoader(dataset, batch_size=2, shuffle=True)


# 5단계: 학습 루프
num_epochs = 100  # 학습할 에폭 수를 10으로 설정

model = VoiceConversionModel()
criterion = nn.MSELoss()  # Mean Squared Error 손실 함수 사용
optimizer = torch.optim.Adam(model.parameters(), lr=0.001)  # Adam 옵티마이저 사용

for epoch in range(num_epochs):
    for source_waveform, target_waveform in dataloader:
        optimizer.zero_grad()  # 옵티마이저의 그라디언트를 0으로 초기화
        output = model(source_waveform)  # 모델에 소스 음성을 입력하여 출력 생성
        loss = criterion(output, target_waveform)  # 출력과 타겟 음성 사이의 손실 계산
        loss.backward()  # 손실 함수의 그라디언트 계산
        optimizer.step()  # 모델의 파라미터 업데이트
    print(f'Epoch [{epoch+1}/{num_epochs}], Loss: {loss.item()}')  # 현재 에폭과 손실 출력


# 1. 오디오 파일 로딩 및 전처리
# 예시: 새로운 음성 파일 로드 및 전처리
def preprocess_audio(file_path, target_sr=16000):
    waveform, sr = torchaudio.load(file_path)
    # 재샘플링
    if sr != target_sr:
        resampler = T.Resample(sr, target_sr)
        waveform = resampler(waveform)
               
    # 모노 채널로 변환 (필요한 경우)
    if waveform.size(0) > 1:
        waveform = torch.mean(waveform, dim=0, keepdim=True)
    return waveform

source_waveform = preprocess_audio("D:/대학원/librosa/source_voice.mp3")

# 모델을 사용한 예측 수행
# 모델을 평가 모드로 설정
model.eval()

# 예측 수행 (불필요한 그라디언트 계산 방지)
with torch.no_grad():
    output_waveform = model(source_waveform[None, :])  # 차원 추가: [C, L] -> [N, C, L]

# 결과 출력(저장)하기
output_waveform = output_waveform.squeeze()  # 차원 축소: [N, C, L] -> [C, L]
torchaudio.save("D:/대학원/librosa/result.wav", output_waveform.cpu(), 16000)
