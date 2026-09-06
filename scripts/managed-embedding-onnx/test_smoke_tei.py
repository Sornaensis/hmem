from __future__ import annotations

from typing import Any
from unittest import mock

import numpy as np

import smoke_tei


def main() -> None:
    texts = [f"item-{index}" for index in range(11)]
    calls: list[dict[str, Any]] = []

    def fake_request(
        url: str, path: str, body: dict[str, Any] | None, timeout: float
    ) -> tuple[int, Any, float]:
        assert url == "http://127.0.0.1:80"
        assert path == "/embed"
        assert body is not None
        batch = body["inputs"]
        assert 1 <= len(batch) <= 8
        calls.append(body)
        return 200, [[text] for text in batch], float(len(batch))

    with mock.patch.object(smoke_tei, "request", side_effect=fake_request):
        vectors, latencies = smoke_tei.request_embedding_batches(
            "http://127.0.0.1:80", texts, True, 30
        )

    assert [vector[0] for vector in vectors] == texts
    assert [len(call["inputs"]) for call in calls] == [8, 3]
    assert all(call["normalize"] is True and call["truncate"] is False for call in calls)
    assert latencies == [8.0, 3.0]

    identical = np.asarray([3.0, 4.0], dtype=np.float32)
    raw_result = smoke_tei.compare(
        identical,
        identical,
        {"absolute_tolerance": 1e-4, "relative_tolerance": 1e-4},
    )
    assert raw_result["pass"] is True
    normalized_result = smoke_tei.compare(
        identical / 5.0,
        identical / 5.0,
        {
            "absolute_tolerance": 1e-6,
            "relative_tolerance": 1e-6,
            "max_cosine_distance": 1e-6,
        },
    )
    assert normalized_result["pass"] is True

    token_body = [[{"id": 1}] * 32768 + [{"id": 151643}]]
    assert len(smoke_tei.tokenize_ids(token_body, 32769)) == 32769
    error_body = {"error": smoke_tei.OVER_LIMIT_ERROR, "error_type": "validation"}
    assert smoke_tei.is_over_limit_embed_rejection(422, error_body) is True
    assert smoke_tei.is_over_limit_embed_rejection(503, error_body) is False
    try:
        smoke_tei.tokenize_ids(error_body, 32769)
    except ValueError:
        pass
    else:
        raise AssertionError("a /tokenize rejection was accepted as token-count evidence")


if __name__ == "__main__":
    main()
