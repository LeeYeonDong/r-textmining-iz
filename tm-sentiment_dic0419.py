# Hugging Face의 transformers 라이브러리와 pipeline 기능을 사용하여 한국어 감정 분석을 수행하려면, 우선적으로 한국어를 지원하는 사전 훈련된 모델을 선택해야 합니다. 
from transformers import pipeline, AutoTokenizer, AutoModelForSequenceClassification

# KoELECTRA 모델과 토크나이저를 불러옵니다.
tokenizer = AutoTokenizer.from_pretrained("monologg/koelectra-small-v3-discriminator")
model = AutoModelForSequenceClassification.from_pretrained("monologg/koelectra-small-v3-discriminator")

# 사전 훈련된 감정 분석 파이프라인을 초기화합니다.
nlp = pipeline('sentiment-analysis', model=model, tokenizer=tokenizer)

# 분석하고자 하는 한국어 문장입니다.
sentence = "나는 맑은 날이 좋지만 비는 싫어."

# 모델을 사용하여 문장의 감정을 분석합니다.
result = nlp(sentence)

print(result)
# [{'label': 'LABEL_0', 'score': 0.5002934336662292}]