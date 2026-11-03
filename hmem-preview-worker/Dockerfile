FROM python:3.13-slim-bookworm@sha256:281d39ff02e3b389401586134ff9e6572954687822a58055ec07a60ded4e0b7f
ENV PYTHONDONTWRITEBYTECODE=1 PYTHONUNBUFFERED=1
WORKDIR /app
COPY requirements.lock /app/requirements.lock
RUN python -m pip install --no-cache-dir --require-hashes -r /app/requirements.lock \
    && rm /app/requirements.lock
COPY preview_worker /app/preview_worker
USER 65534:65534
ENTRYPOINT ["python", "-m", "preview_worker.worker"]
