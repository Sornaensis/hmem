FROM python@sha256:2856e6af199e8128161abd320575eb9b341f3b76f017b5d0c9cd364f60d8a050

ENV PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PIP_NO_CACHE_DIR=1 \
    PYTHONHASHSEED=0 \
    TOKENIZERS_PARALLELISM=false

WORKDIR /work
COPY requirements.lock /tmp/requirements.lock
RUN python -m pip install --require-hashes --only-binary=:all: -r /tmp/requirements.lock

ENTRYPOINT ["python"]
